CLASS ZCL_mds_api_ds DEFINITION
  PUBLIC
  ABSTRACT
  CREATE PROTECTED.

  PUBLIC SECTION.
    INTERFACES ZIF_mds_api_datasource.
    ALIASES: relation_cust FOR ZIF_mds_api_datasource~relation_cust.

    CLASS-METHODS class_constructor.
    CLASS-METHODS get_ds_instance IMPORTING i_ds_id              TYPE zmds_ds_id
                                  RETURNING VALUE(e_ds_instance) TYPE REF TO ZIF_mds_api_datasource
                                  RAISING   zcx_mds_id.

    METHODS constructor IMPORTING i_sematic_key TYPE zmds_ds_semkey.

  PROTECTED SECTION.

    CLASS-DATA instance TYPE REF TO ZCL_mds_api_ds.

    TYPES: BEGIN OF ty_ds_field,
             field_name             TYPE fieldname,
             field_alias            TYPE fieldname_raw,
             position               TYPE ddfdpos,
             base_table             TYPE vibastab,
             base_field_name        TYPE vibasfld,
             origin_field_name      TYPE ddfieldname_l,
             origin_appendstru_name TYPE appname,
             datatype               TYPE datatype_d,
             inttype                TYPE inttype,
             length                 TYPE zmds_datatype_length,
             data_element           TYPE rollname,
           END OF ty_ds_field,
           ty_ds_fields TYPE STANDARD TABLE OF ty_ds_field.

    TYPES: BEGIN OF ty_ds_instance,
             ds_id    TYPE zmds_ds_id,
             instance TYPE REF TO ZIF_mds_api_datasource,
           END OF ty_ds_instance,
           ty_ds_instances TYPE SORTED TABLE OF ty_ds_instance WITH UNIQUE KEY ds_id.

    CONSTANTS: BEGIN OF version,
                 active TYPE as4local VALUE 'A',
               END OF version.

    CLASS-DATA ds_instances TYPE ty_ds_instances.
    CLASS-DATA id_handler TYPE REF TO ZCL_mds_id.

    DATA: ds_fields     TYPE ty_ds_fields.
    DATA: related_read  TYPE abap_bool.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_MDS_API_DS IMPLEMENTATION.


  METHOD ZIF_mds_api_datasource~build_related_entities.

  ENDMETHOD.


  METHOD ZIF_mds_api_datasource~get_action_links.

    TRY.

        DATA(wb_object) = cl_wb_object=>create_from_transport_key( p_object   = SWITCH #( me->ZIF_mds_api_datasource~header-type
                                                                                          WHEN 'YABL' THEN 'TABL'
                                                                                          WHEN 'YDLS' THEN 'DDLS'
                                                                                          ELSE me->ZIF_mds_api_datasource~header-type )
                                                                           p_obj_name = CONV #( me->ZIF_mds_api_datasource~header-name ) ).
        DATA(adt_objref) = cl_adt_tools_core_factory=>get_instance( )->get_uri_mapper( )->map_wb_object_to_objref( wb_object ).
        r_links_action-edit = |{ 'adt://' }{ to_lower( sy-sysid ) }{ adt_objref->ref_data-uri }|.

      CATCH cx_root.
    ENDTRY.

  ENDMETHOD.


  METHOD ZIF_mds_api_datasource~get_annotations.

  ENDMETHOD.


  METHOD ZIF_mds_api_datasource~get_datasource.
    r_datasource = me->ZIF_mds_api_datasource~header.
  ENDMETHOD.


  METHOD ZIF_mds_api_datasource~get_fields.

  ENDMETHOD.


  METHOD ZIF_mds_api_datasource~get_parameters.

  ENDMETHOD.


  METHOD ZIF_mds_api_datasource~get_properties.

    APPEND VALUE #( property_id = ZCL_mds_api=>build_object_id( VALUE zmds_py_semkey( object_id     = me->ZIF_mds_api_datasource~header-ds_id
                                                                                                    property_name = 'DSID' ) )
                    object_id   = me->ZIF_mds_api_datasource~header-ds_id ) TO r_properties.
  ENDMETHOD.


  METHOD ZIF_mds_api_datasource~get_relations.
    r_relations = me->ZIF_mds_api_datasource~relations.
  ENDMETHOD.


  METHOD ZIF_mds_api_datasource~has_field.

*    IF me->ZIF_mds_api_datasource~header-name = 'ZCDX_E_PARTNER'.
*      BREAK-POINT.
*    ENDIF.

    IF i_fieldname_search-search_object_name IS NOT INITIAL.
      IF me->ZIF_mds_api_datasource~header-name <> i_fieldname_search-search_object_name
         AND me->ZIF_mds_api_datasource~header-sqlviewname <> i_fieldname_search-search_object_name.
        RETURN.
      ENDIF.
    ENDIF.
    IF me->ds_fields IS INITIAL.
      me->ZIF_mds_api_datasource~get_fields( ).
    ENDIF.

    IF line_exists( ds_fields[ field_name = i_fieldname_search-search_field_name ] ).
      ASSIGN ds_fields[ field_name = i_fieldname_search-search_field_name ] TO FIELD-SYMBOL(<field>).
    ELSEIF line_exists( ds_fields[ field_alias = i_fieldname_search-search_field_name ] ).
      ASSIGN ds_fields[ field_alias = i_fieldname_search-search_field_name ] TO <field>.
    ENDIF.

    IF <field> IS ASSIGNED.
      me->ZIF_mds_api_datasource~header-field_search-search_field_name = <field>-field_name.
      IF me->ZIF_mds_api_datasource~header-sqlviewname IS NOT INITIAL.
        ZIF_mds_api_datasource~header-field_search-search_object_name = me->ZIF_mds_api_datasource~header-sqlviewname.
      ELSE.
        ZIF_mds_api_datasource~header-field_search-search_object_name = me->ZIF_mds_api_datasource~header-name.
      ENDIF.

      IF <field>-origin_field_name IS NOT INITIAL AND <field>-origin_appendstru_name IS NOT INITIAL.
        ZIF_mds_api_datasource~header-field_search-base_field_name = <field>-origin_field_name.
      ELSEIF <field>-base_field_name IS NOT INITIAL AND <field>-base_table IS NOT INITIAL.
        ZIF_mds_api_datasource~header-field_search-base_field_name = <field>-base_field_name.
      ELSE.
        ZIF_mds_api_datasource~header-field_search-base_field_name = i_fieldname_search-search_field_name.
      ENDIF.
      IF <field>-origin_appendstru_name IS NOT INITIAL.
        ZIF_mds_api_datasource~header-field_search-base_object_name = <field>-origin_appendstru_name.
      ELSEIF <field>-base_table IS NOT INITIAL.
        ZIF_mds_api_datasource~header-field_search-base_object_name = <field>-base_table.
      ELSE.
        ZIF_mds_api_datasource~header-field_search-base_object_name = me->ZIF_mds_api_datasource~header-name.
      ENDIF.
    ENDIF.
    r_field_source_ds = ZIF_mds_api_datasource~header-field_search.
  ENDMETHOD.


  METHOD ZIF_mds_api_datasource~set_role.

    me->ZIF_mds_api_datasource~header-depth = 0.
    IF me->ZIF_mds_api_datasource~header-role = ZIF_mds_api=>ds_role-main.
      me->ZIF_mds_api_datasource~header-role  = i_role.
    ENDIF.

  ENDMETHOD.


  METHOD ZIF_mds_api_datasource~uses_field.

    IF me->ds_fields IS INITIAL.
      me->ZIF_mds_api_datasource~get_fields( ).
    ENDIF.

    IF line_exists( ds_fields[ base_field_name = i_fieldname_search-base_field_name
                               base_table      = i_fieldname_search-base_object_name ] ).
      ASSIGN ds_fields[ base_field_name = i_fieldname_search-base_field_name
                        base_table      = i_fieldname_search-base_object_name ] TO FIELD-SYMBOL(<field>).

      me->ZIF_mds_api_datasource~header-field_search-search_field_name = <field>-field_name.
      IF me->ZIF_mds_api_datasource~header-sqlviewname IS NOT INITIAL.
        ZIF_mds_api_datasource~header-field_search-search_object_name = me->ZIF_mds_api_datasource~header-sqlviewname.
      ELSE.
        ZIF_mds_api_datasource~header-field_search-search_object_name = me->ZIF_mds_api_datasource~header-name.
      ENDIF.
      IF <field>-base_field_name IS NOT INITIAL.
        ZIF_mds_api_datasource~header-field_search-base_field_name = <field>-base_field_name.
      ELSE.
        ZIF_mds_api_datasource~header-field_search-base_field_name = i_fieldname_search-search_field_name.
      ENDIF.
      IF <field>-base_table IS NOT INITIAL.
        ZIF_mds_api_datasource~header-field_search-base_object_name = <field>-base_table.
      ELSE.
        ZIF_mds_api_datasource~header-field_search-base_object_name = me->ZIF_mds_api_datasource~header-name.
      ENDIF.
    ENDIF.
    r_field_source_ds = ZIF_mds_api_datasource~header-field_search.

  ENDMETHOD.


  METHOD class_constructor.
    id_handler = ZCL_mds_id=>get_instance( ).
  ENDMETHOD.


  METHOD constructor.

    me->ZIF_mds_api_datasource~header-semkey = i_sematic_key.
    me->ZIF_mds_api_datasource~header-ds_id = ZCL_mds_api=>build_object_id( me->ZIF_mds_api_datasource~header-semkey ).
    me->ZIF_mds_api_datasource~header-api = me.

  ENDMETHOD.


  METHOD get_ds_instance.

    DATA: ds_instance TYPE REF TO ZIF_mds_api_datasource.

    IF NOT line_exists( ds_instances[ ds_id = i_ds_id ] ).


      DATA(semkey) = id_handler->get_ds_semkey( i_ds_id ).

      DATA(ds_class_name) = 'ZCL_MDS_API_DS_' && semkey-type.


      CREATE OBJECT ds_instance TYPE (ds_class_name) EXPORTING i_sematic_key = semkey.


      IF ds_instance IS INITIAL.
        ds_instance->set_role( ZIF_mds_api=>ds_role-main ).
      ENDIF.

      INSERT VALUE #( ds_id    = i_ds_id
                      instance = ds_instance ) INTO TABLE ds_instances ASSIGNING FIELD-SYMBOL(<instance>).


    ELSE.
      ASSIGN ds_instances[ ds_id = i_ds_id ] TO <instance>.
    ENDIF.

    e_ds_instance = <instance>-instance.

  ENDMETHOD.
ENDCLASS.
