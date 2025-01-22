CLASS ZCL_mds_api DEFINITION
  PUBLIC
  CREATE PROTECTED.

  PUBLIC SECTION.
    INTERFACES ZIF_mds_api.

    CLASS-METHODS get_instance RETURNING VALUE(e_api) TYPE REF TO ZIF_mds_api.

    CLASS-METHODS class_constructor.
    CLASS-METHODS build_object_id IMPORTING i_semkey    TYPE any
                                  RETURNING VALUE(e_id) TYPE zmds_object_id.
  PROTECTED SECTION.

    CLASS-DATA instance TYPE REF TO ZIF_mds_api.
    CLASS-DATA id_handler TYPE REF TO ZCL_mds_id.

    TYPES: BEGIN OF ty_recusion,
             ds_name     TYPE zmds_object_name,
             datasources TYPE ZIF_mds_api=>ty_datasources,
           END OF ty_recusion,
           ty_recusions TYPE STANDARD TABLE OF ty_recusion.
    DATA: recursions TYPE ty_recusions.

    METHODS get_ds_reader IMPORTING i_ds_id            TYPE zmds_ds_id
                          RETURNING VALUE(r_ds_reader) TYPE REF TO ZIF_mds_api_datasource
                          RAISING
                                    zcx_mds_id.
    METHODS search_field IMPORTING is_role           LIKE ZIF_mds_api=>ds_role-child
                         CHANGING  c_field_source_ds TYPE zmds_field_search
                                   c_related_ds      TYPE ZIF_mds_api=>ty_datasource.
ENDCLASS.



CLASS ZCL_mds_api IMPLEMENTATION.


  METHOD ZIF_mds_api~get_annotations_by_dsid.

    DATA(ds_reader) = me->get_ds_reader( i_ds_id ).

    r_annotations = ds_reader->get_annotations( ).

    DELETE r_annotations WHERE object_id <> i_ds_id.

  ENDMETHOD.


  METHOD ZIF_mds_api~get_annotations_by_fieldid.

    SELECT SINGLE ds_id
           FROM zmds_fd
           WHERE field_id = @i_field_id
           INTO @DATA(semkey).
    IF sy-subrc <> 0.
      MESSAGE '' TYPE 'X'.
    ENDIF.

    DATA(ds_reader) = me->get_ds_reader( semkey ).

    r_annotations = ds_reader->get_annotations( ).

    DELETE r_annotations WHERE object_id <> i_field_id.

  ENDMETHOD.


  METHOD ZIF_mds_api~get_annotation_by_id.

    SELECT SINGLE object_id, annotation
           FROM zmds_an
           WHERE annotation_id = @i_annotation_id
           INTO @DATA(semkey).
    IF sy-subrc <> 0.
      MESSAGE '' TYPE 'X'.
    ENDIF.

    DATA(annotations) = ZIF_mds_api~get_annotations_by_dsid( semkey-object_id ).

    r_annotation = annotations[ annotation_id = i_annotation_id ].

  ENDMETHOD.


  METHOD ZIF_mds_api~get_datasources_by_id.

    DATA(ds_reader) = me->get_ds_reader( i_ds_id ).
    ds_reader->set_role( i_as_role ).
    DATA(ds_name) = ds_reader->get_datasource( )-name.

    IF ds_reader->header-role = ZIF_mds_api~ds_role-main.
      APPEND VALUE #( ds_name = ds_name ) TO me->recursions ASSIGNING FIELD-SYMBOL(<recursion>).
    ELSE.
      IF line_exists( me->recursions[ ds_name = ds_name ] ).
        IF i_as_role = ZIF_mds_api~ds_role-main.
          "2nd/independent request
          r_datasources = me->recursions[ ds_name = ds_name ]-datasources.
        ENDIF.
        "recursive Call
        RETURN.

      ELSE.
        APPEND VALUE #( ds_name = ds_name ) TO me->recursions ASSIGNING <recursion>.
      ENDIF.

    ENDIF.

    IF i_filter_fieldname IS NOT INITIAL.

      IF i_filter_datasource IS INITIAL OR i_filter_datasource = ds_name.

        DATA(field_source_ds) = ds_reader->has_field( VALUE #( search_field_name = i_filter_fieldname ) ).
        TYPES: ty_search_fields TYPE STANDARD TABLE OF zmds_field_search WITH DEFAULT KEY.
        DATA(field_child_source_dss)  = VALUE ty_search_fields(  ).
        DATA(field_parent_source_dss) = VALUE ty_search_fields(  ).

      ELSE.

        DATA(other_search_source) = abap_true.

      ENDIF.

    ENDIF.

    APPEND ds_reader->get_datasource( ) TO r_datasources.

    ds_reader->build_related_entities( ).

    DATA(relations) = ds_reader->get_relations( ).

    LOOP AT relations ASSIGNING FIELD-SYMBOL(<relation>).

      IF <relation>-relation_type = ZIF_mds_api_datasource=>relation_cust-isused-type.
        DATA(as_role) = ZIF_mds_api=>ds_role-child.
        ASSIGN field_child_source_dss TO FIELD-SYMBOL(<field_source_dss>).
      ELSE.
        as_role = ZIF_mds_api=>ds_role-parent.
        ASSIGN field_parent_source_dss TO <field_source_dss>.
      ENDIF.

      CLEAR <field_source_dss>.
      IF field_source_ds IS NOT INITIAL.
        APPEND field_source_ds TO <field_source_dss>.
      ENDIF.


      DATA(related_dss) = ZIF_mds_api~get_datasources_by_id( i_ds_id   = <relation>-object_id2
                                                                    i_as_role = as_role ).

*      IF lines( related_dss ) > 4.
*        BREAK-POINT.
*      ENDIF.
      LOOP AT related_dss ASSIGNING FIELD-SYMBOL(<related_ds>).

        IF NOT line_exists( r_datasources[ ds_id = <related_ds>-ds_id ] ).

          LOOP AT <field_source_dss> ASSIGNING FIELD-SYMBOL(<field_source_ds>).
            DATA(field_source) = <field_source_ds>.
            me->search_field( EXPORTING is_role           = as_role
                              CHANGING  c_field_source_ds = field_source
                                        c_related_ds      = <related_ds> ).
            IF  field_source <> <field_source_ds> AND
              (
               field_source-search_object_name <> field_source-base_object_name OR
               field_source-search_field_name  <> field_source-base_field_name
                ).

              IF as_role = ZIF_mds_api=>ds_role-child.
                APPEND field_source TO <field_source_dss>.
              ELSE.
                <field_source_ds> = field_source.
              ENDIF.
              EXIT. "LOOP
            ENDIF.
          ENDLOOP.

          APPEND <related_ds> TO r_datasources.

        ENDIF.

      ENDLOOP.

    ENDLOOP.

    IF other_search_source = abap_true.
      ASSIGN r_datasources[ name = i_filter_datasource ] TO FIELD-SYMBOL(<filter_datasource>).
      IF sy-subrc = 0.
        DATA(search_dss) = ZCL_mds_search_api=>get_search_instance( )->get_datasources_by_semkey( i_ds_semkey        = VALUE #( name = <filter_datasource>-name type = <filter_datasource>-type )
                                                                                                         i_filter_fieldname = i_filter_fieldname ).
        LOOP AT r_datasources ASSIGNING FIELD-SYMBOL(<datasource>).
          ASSIGN search_dss[ ds_id = <datasource>-ds_id ] TO FIELD-SYMBOL(<search_datasource>).
          IF sy-subrc = 0.
            <datasource>-field_search = <search_datasource>-field_search.
          ENDIF.
        ENDLOOP.
      ENDIF.
    ENDIF.

    <recursion>-datasources = r_datasources.

  ENDMETHOD.


  METHOD search_field.

    IF is_role = ZIF_mds_api=>ds_role-parent.
      c_field_source_ds = VALUE zmds_field_search( BASE c_field_source_ds search_object_name = c_field_source_ds-base_object_name
                                                                                 search_field_name  = c_field_source_ds-base_field_name ).

      DATA(field_source_related_ds) = c_related_ds-api->has_field( c_field_source_ds ).

    ELSEIF is_role = ZIF_mds_api=>ds_role-child.
      c_field_source_ds = VALUE #( BASE c_field_source_ds base_object_name = c_field_source_ds-search_object_name
                                                          base_field_name  = c_field_source_ds-search_field_name ).

      field_source_related_ds = c_related_ds-api->uses_field( c_field_source_ds ).
    ENDIF.

    c_related_ds = CORRESPONDING #( BASE ( c_related_ds ) field_source_related_ds ).

    IF field_source_related_ds IS NOT INITIAL.
      c_field_source_ds = field_source_related_ds.
    ENDIF.

  ENDMETHOD.


  METHOD ZIF_mds_api~get_datasources_by_semkey.

    DATA(ds_semkey) = i_ds_semkey.
    ds_semkey-name = to_upper( ds_semkey-name ).
    ds_semkey-type = to_upper( ds_semkey-type ).

    DATA(id) = me->build_object_id( ds_semkey ).

    r_datasources = me->ZIF_mds_api~get_datasources_by_id( i_ds_id             = id
                                                                  i_filter_fieldname  = i_filter_fieldname
                                                                  i_filter_datasource = i_filter_datasource ).

  ENDMETHOD.


  METHOD ZIF_mds_api~get_datasource_by_id.

    DATA(datasources) = ZIF_mds_api~get_datasources_by_id( i_ds_id ).

    TRY.
        r_datasource = datasources[ ds_id = i_ds_id ].
      CATCH cx_root.
        BREAK-POINT.
    ENDTRY.
  ENDMETHOD.


  METHOD ZIF_mds_api~get_fields_by_dsid.

    DATA(ds_reader) = me->get_ds_reader( i_ds_id ).

    ds_reader->build_related_entities( ).

    r_fields = ds_reader->get_fields( ).

  ENDMETHOD.


  METHOD ZIF_mds_api~get_field_by_id.

    SELECT SINGLE ds_id, field_name
           FROM zmds_fd
           WHERE field_id = @i_field_id
           INTO @DATA(semkey).
    IF sy-subrc <> 0.
      MESSAGE '' TYPE 'X'.
    ENDIF.

    DATA(fields) = ZIF_mds_api~get_fields_by_dsid( semkey-ds_id ).

    r_field = fields[ field_id = i_field_id ].

  ENDMETHOD.


  METHOD ZIF_mds_api~get_links_by_dsid.

    DATA(ds_reader) = me->get_ds_reader( i_ds_id ).

    ds_reader->build_related_entities( ).

    r_relations = ds_reader->get_relations( ).

  ENDMETHOD.


  METHOD ZIF_mds_api~get_link_by_id.

    DATA(links) = ZIF_mds_api~get_links_by_dsid( id_handler->get_link_semkey( i_link_id )-object_id1 ).

    r_link = links[ link_id = i_link_id ].

  ENDMETHOD.


  METHOD ZIF_mds_api~get_parameters_by_dsid.

    DATA(ds_reader) = me->get_ds_reader( i_ds_id ).

    ds_reader->build_related_entities( ).

    r_parameters = ds_reader->get_parameters( ).

  ENDMETHOD.


  METHOD ZIF_mds_api~get_parameter_by_id.

    DATA(parameters) = ZIF_mds_api~get_parameters_by_dsid( id_handler->get_parameter_semkey( i_parameter_id )-ds_id ).

    r_parameter = parameters[ parameter_id = i_parameter_id ].

  ENDMETHOD.


  METHOD build_object_id.

    e_id = id_handler->build_hash( i_semkey ).

  ENDMETHOD.

  METHOD class_constructor.

    id_handler = ZCL_mds_id=>get_instance( ).

  ENDMETHOD.


  METHOD get_ds_reader.

    r_ds_reader = ZCL_mds_api_ds=>get_ds_instance( i_ds_id ).

  ENDMETHOD.


  METHOD get_instance.

    IF instance IS INITIAL.
      instance = NEW ZCL_mds_api( ).
    ENDIF.

    e_api = instance.

  ENDMETHOD.


  METHOD ZIF_mds_api~get_properties_by_dsid.

  ENDMETHOD.

  METHOD ZIF_mds_api~get_properties_by_fieldid.

  ENDMETHOD.

  METHOD ZIF_mds_api~get_property_by_id.

  ENDMETHOD.

  METHOD ZIF_mds_api~get_dashboard_data.
    DATA lv_sum TYPE i.

    " Tables
    IF i_custom_objects = abap_true.
      DATA(lv_custom_objects) = |( tabname LIKE 'Z%' OR tabname LIKE 'Y%' )|.
    ELSE.
      lv_custom_objects = |( tabname NOT LIKE 'Z%' AND tabname NOT LIKE 'Y%' )|.
    ENDIF.

    IF i_last_week_data = abap_true.
      DATA(lv_last_week) = sy-datum - 7.
      DATA(lv_last_week_data) = |AS4DATE >= @lv_last_week|.
    ENDIF.

    SELECT COUNT( * ) FROM DD02L WHERE
        TABCLASS = 'TRANSP'
        AND (lv_custom_objects)
        AND (lv_last_week_data) INTO @DATA(lv_tables_count).

    lv_sum = lv_sum + lv_tables_count.

    "CDS Views
    CLEAR lv_custom_objects.
    IF i_custom_objects = abap_true.
      lv_custom_objects = |( strucobjn LIKE 'Z%' OR strucobjn LIKE 'Y%' )|.
    ELSE.
      lv_custom_objects = |( strucobjn NOT LIKE 'Z%' AND strucobjn NOT LIKE 'Y%' )|.
    ENDIF.

    CLEAR lv_last_week_data.
    IF i_last_week_data = abap_true.
      lv_last_week_data = |chgdate >= @lv_last_week|.
    ENDIF.

    SELECT COUNT( * ) FROM dd02b WHERE
        strucobjclass = @space
        AND (lv_custom_objects)
        AND (lv_last_week_data) INTO @DATA(lv_cds_views_count).

    lv_sum = lv_sum + lv_cds_views_count.

    "CDS Views Extensions
    SELECT COUNT( * ) FROM dd02b WHERE
        strucobjclass = 'APPEND'
        AND (lv_custom_objects)
        AND (lv_last_week_data) INTO @DATA(lv_cds_ext_count).

    lv_sum = lv_sum + lv_cds_ext_count.

    "SQL Views
    CLEAR lv_custom_objects.
    IF i_custom_objects = abap_true.
      lv_custom_objects = |( a~strucobjn LIKE 'Z%' OR a~strucobjn LIKE 'Y%' )|.
    ELSE.
      lv_custom_objects = |( a~strucobjn NOT LIKE 'Z%' AND a~strucobjn NOT LIKE 'Y%' )|.
    ENDIF.

    CLEAR lv_last_week_data.
    IF i_last_week_data = abap_true.
      lv_last_week_data = |b~AS4DATE >= @lv_last_week|.
    ENDIF.

    SELECT COUNT( * ) FROM DD02BND as a
     INNER JOIN dd02l as b ON a~dbtabname = b~tabname
     WHERE
        dbobjkind = 'VIEW'
        AND (lv_custom_objects)
        AND (lv_last_week_data) INTO @DATA(lv_sql_views_count).

    lv_sum = lv_sum + lv_sql_views_count.

      r_dashobard_data = VALUE #(
    ( object_name = 'Database Tables' object_type = 'TABL'   count = lv_tables_count total_count = lv_sum )
    ( object_name = 'CDS Views' object_type = 'DDLS'   count = lv_cds_views_count total_count = lv_sum )
    ( object_name = 'CDS View Extenstions' object_type = 'DDLX'   count = lv_cds_ext_count total_count = lv_sum )
    ( object_name = 'SQL Views' object_type = 'DDLX'   count = lv_sql_views_count total_count = lv_sum )
   ).
  ENDMETHOD.

ENDCLASS.
