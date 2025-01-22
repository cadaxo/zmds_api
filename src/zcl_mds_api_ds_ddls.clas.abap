CLASS ZCL_mds_api_ds_ddls DEFINITION INHERITING FROM ZCL_mds_api_ds
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.
    CLASS-METHODS class_constructor.
    METHODS constructor IMPORTING i_sematic_key TYPE zmds_ds_semkey
                        RAISING   zcx_mds_id.
    METHODS ZIF_mds_api_datasource~build_related_entities REDEFINITION.
    METHODS ZIF_mds_api_datasource~get_fields REDEFINITION.
    METHODS ZIF_mds_api_datasource~get_parameters REDEFINITION.
    METHODS ZIF_mds_api_datasource~get_annotations REDEFINITION.
    METHODS ZIF_mds_api_datasource~get_action_links REDEFINITION.

  PROTECTED SECTION.
    CLASS-DATA: metadata_extension_available TYPE abap_bool.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_MDS_API_DS_DDLS IMPLEMENTATION.


  METHOD ZIF_mds_api_datasource~build_related_entities.

    IF me->related_read = abap_false.

      IF me->ZIF_mds_api_datasource~header-role <= ZIF_mds_api=>ds_role-main.
* get associations
        SELECT associationname, typekind_t, assorigin, strucobjn_t, card_min, card_max
          FROM dd08b
          INTO TABLE @DATA(stob_associations)
          WHERE strucobjn = @me->ZIF_mds_api_datasource~header-name
            AND as4local  = @version-active.

        LOOP AT stob_associations ASSIGNING FIELD-SYMBOL(<stob_association>).

          APPEND VALUE #( link_id       = 'GET_ID'
                          object_id1    = me->ZIF_mds_api_datasource~header-ds_id
                          object_id2    = ZCL_mds_api=>build_object_id( VALUE zmds_ds_semkey( type = SWITCH #( <stob_association>-typekind_t
                                                                                                                                WHEN 'T' THEN ZIF_mds_api_datasource~type-table
                                                                                                                                WHEN 'B' THEN ZIF_mds_api_datasource~type-datadefinition )
                                                                                                            name = <stob_association>-strucobjn_t ) )
                          description   = <stob_association>-associationname
                          card_min      = <stob_association>-card_min
                          card_max      = <stob_association>-card_max
                          relation_type = SWITCH #( <stob_association>-assorigin WHEN 'E' THEN 'EXTERNAL_ASSOCIATION'
                                                                                          ELSE 'ASSOCIATION' ) ) TO me->ZIF_mds_api_datasource~relations.

        ENDLOOP.


*    IF me->ZIF_mds_api_datasource~header-name = 'ZCDX_ORDER_CDS_00'.
*      BREAK-POINT.
*    ENDIF.

* get enhancements
        SELECT strucobjn
               FROM dd02b
               WHERE parentname    = @me->ZIF_mds_api_datasource~header-name
                 AND as4local      = @version-active
                 AND strucobjclass = 'APPEND'
               INTO TABLE @DATA(stob_append_headers).

        LOOP AT stob_append_headers ASSIGNING FIELD-SYMBOL(<stob_append_header>).

          APPEND VALUE #( link_id       = 'GET_ID'
                          object_id1    = me->ZIF_mds_api_datasource~header-ds_id
                          object_id2    = ZCL_mds_api=>build_object_id( VALUE zmds_ds_semkey( type = ZIF_mds_api_datasource~type-enhancment
                                                                                                            name = <stob_append_header>-strucobjn ) )
                          card_min      = 1
                          card_max      = 1
                          description   = relation_cust-enhancement-description
                          relation_type = relation_cust-enhancement-type ) TO me->ZIF_mds_api_datasource~relations.

        ENDLOOP.

* Base Tables
*
        SELECT base~tabname AS basetable, issqlview~strucobjn AS basecdsview
               FROM dd26s AS base
               LEFT OUTER JOIN dd02bnd AS issqlview
                 ON  issqlview~dbtabname = base~tabname
                 AND issqlview~as4local  = base~as4local
               WHERE base~viewname = @ZIF_mds_api_datasource~header-sqlviewname
                 AND base~as4local = @version-active
               INTO TABLE @DATA(bases).

        LOOP AT bases ASSIGNING FIELD-SYMBOL(<base>).

          IF <base>-basecdsview IS INITIAL.
            DATA(base_semkey) = VALUE zmds_ds_semkey( type = ZIF_mds_api_datasource~type-table
                                                             name = <base>-basetable ).
          ELSE.
            base_semkey = VALUE zmds_ds_semkey( type = ZIF_mds_api_datasource~type-datadefinition
                                                       name = <base>-basecdsview ).

          ENDIF.

          APPEND VALUE #( link_id       = 'GET_ID'
                          object_id1    = me->ZIF_mds_api_datasource~header-ds_id
                          object_id2    = ZCL_mds_api=>build_object_id( base_semkey )
                          description   = relation_cust-base-description
                          relation_type = relation_cust-base-type ) TO me->ZIF_mds_api_datasource~relations.

        ENDLOOP.


* get extensions

        IF metadata_extension_available = abap_true.
          DATA(from_clause) = 'DDLX_RT_HEADER AS header INNER JOIN ddlxsrc AS source ON source~uuid = header~dt_uuid AND source~ddlxname = header~ddlxname'.
          DATA(where_clause) = 'header~extended_artifact = @me->ZIF_mds_api_datasource~header-name AND source~version = @version-active'.
          DATA: metadataextensions TYPE TABLE OF string.

          SELECT ('HEADER~DDLXNAME')
                 FROM (from_clause)
              INTO TABLE @metadataextensions
              WHERE (where_clause).


          LOOP AT metadataextensions ASSIGNING FIELD-SYMBOL(<extension>).

            APPEND VALUE #( link_id       = 'GET_ID'
                            object_id1    = me->ZIF_mds_api_datasource~header-ds_id
                            object_id2    = ZCL_mds_api=>build_object_id( VALUE zmds_ds_semkey(  type = ZIF_mds_api_datasource~type-metadataextension
                                                                                                               name = <extension> ) )
                            card_min      = 1
                            card_max      = 1
                            description   = relation_cust-enhancement-description
                            relation_type = relation_cust-enhancement-type ) TO me->ZIF_mds_api_datasource~relations.
          ENDLOOP.

        ENDIF.

      ENDIF.

      IF me->ZIF_mds_api_datasource~header-role >= ZIF_mds_api=>ds_role-main.
* is base table of
        SELECT b~strucobjn
               FROM dd26s AS a
               INNER JOIN dd02bnd AS b
                     ON b~dbtabname = a~viewname
               WHERE a~tabname  = @ZIF_mds_api_datasource~header-sqlviewname
                 AND b~as4local = @version-active
               INTO TABLE @DATA(table_sources).
        SELECT strucobjn
          FROM dd08b
          APPENDING TABLE @table_sources
          WHERE strucobjn_t = @me->ZIF_mds_api_datasource~header-name
            AND as4local  = @version-active.
        LOOP AT table_sources ASSIGNING FIELD-SYMBOL(<table_source>).

          APPEND VALUE #( link_id       = 'GET_ID'
                          object_id1    = me->ZIF_mds_api_datasource~header-ds_id
                          object_id2    = ZCL_mds_api=>build_object_id( VALUE zmds_ds_semkey(  type = ZIF_mds_api_datasource~type-datadefinition
                                                                                                             name = <table_source>-strucobjn ) )
                          description   = relation_cust-isused-description
                          relation_type = relation_cust-isused-type ) TO me->ZIF_mds_api_datasource~relations.

        ENDLOOP.

      ENDIF.


      LOOP AT me->ZIF_mds_api_datasource~relations ASSIGNING FIELD-SYMBOL(<relation>) WHERE link_id = 'GET_ID'.
        <relation>-link_id = ZCL_mds_api=>build_object_id( <relation>-semkey ).
      ENDLOOP.

      me->related_read = abap_true.

    ENDIF.

  ENDMETHOD.


  METHOD ZIF_mds_api_datasource~get_action_links.

    r_links_action = super->ZIF_mds_api_datasource~get_action_links( ).

    r_links_action-display = |/sap/bc/adt/ddic/ddl/sources/{ me->ZIF_mds_api_datasource~header-name }/source/main?version=active&sap-client={ sy-mandt }|.

    IF r_links_action IS INITIAL.
      r_links_action-edit = |adt://{ sy-sysid }/sap/bc/adt/ddic/ddl/sources/{ me->ZIF_mds_api_datasource~header-name }|.
    ENDIF.

  ENDMETHOD.


  METHOD ZIF_mds_api_datasource~get_annotations.

    SELECT name AS annotation, position, value
         FROM ddheadanno
         WHERE strucobjn = @me->ZIF_mds_api_datasource~header-name
         ORDER BY position
         INTO TABLE @DATA(annotations).

    LOOP AT annotations ASSIGNING FIELD-SYMBOL(<annotation>).
      APPEND VALUE #( annotation_id    = ZCL_mds_api=>build_object_id( VALUE zmds_an_semkey( object_id  = me->ZIF_mds_api_datasource~header-ds_id
                                                                                                           annotation = <annotation>-annotation ) )
                      object_id   = me->ZIF_mds_api_datasource~header-ds_id
                      annotation  = <annotation>-annotation
                      value       = <annotation>-value
                      position    = <annotation>-position ) TO r_annotations.
    ENDLOOP.

    SELECT lfieldname AS fieldname, name AS annotation, position, value
           FROM ddfieldanno
           WHERE strucobjn = @me->ZIF_mds_api_datasource~header-name
           ORDER BY position
           INTO TABLE @DATA(field_annotations).
    LOOP AT field_annotations ASSIGNING FIELD-SYMBOL(<field_annotation>).


      DATA(field_id) = ZCL_mds_api=>build_object_id( VALUE zmds_fd_semkey( ds_id      = me->ZIF_mds_api_datasource~header-ds_id
                                                                                         field_name = <field_annotation>-fieldname ) ).
      APPEND VALUE #( annotation_id    = ZCL_mds_api=>build_object_id( VALUE zmds_an_semkey( object_id  = field_id
                                                                                                           annotation = <field_annotation>-annotation ) )
                      object_id   = field_id
                      annotation  = <field_annotation>-annotation
                      value       = <field_annotation>-value
                      position    = <field_annotation>-position ) TO r_annotations.
    ENDLOOP.
  ENDMETHOD.


  METHOD ZIF_mds_api_datasource~get_fields.

    SELECT cdsfields~fieldname AS field_name, cdsfields~fieldname_raw AS field_alias, cdsfields~position,
           sqlviewfields~tabname AS base_table, sqlviewfields~fieldname AS base_field_name,
           cdsfields~fieldname AS origin_field_name,
           cdsfields~keyflag AS is_key,
           cdsfields~appendstruname AS origin_append_stru_name,
           cdsfields~datatype,
           cdsfields~leng,
           cdsfields~decimals,
           cdsfields~rollname AS data_element,
           cdsfields~inttype
           FROM dd03nd AS cdsfields
           INNER JOIN dd27s AS sqlviewfields
                 ON  sqlviewfields~viewfield = cdsfields~fieldname
                 AND sqlviewfields~as4local  = cdsfields~as4local
           WHERE cdsfields~strucobjn     = @me->ZIF_mds_api_datasource~header-name
             AND sqlviewfields~viewname  = @me->ZIF_mds_api_datasource~header-sqlviewname
             AND cdsfields~as4local      = @ZCL_mds_api_ds=>version-active
           ORDER BY position
           INTO TABLE @DATA(fields).


    SELECT texts~fieldname AS field_name,
           texts~ddtext,
           texts~reptext,
           texts~scrtext_s,
           texts~scrtext_m,
           texts~scrtext_l,
           texts~ddlanguage AS spras
           FROM dd03m AS texts
           WHERE tabname    = @me->ZIF_mds_api_datasource~header-sqlviewname
             AND domstat    = @version-active
             AND textstat   = @version-active
             AND ( ddlanguage = @sy-langu OR ddlanguage = 'E' )
           INTO TABLE @DATA(field_texts).

    me->ds_fields = CORRESPONDING #( fields ).

    LOOP AT fields ASSIGNING FIELD-SYMBOL(<ds_field>).

      DATA(field_data) = CORRESPONDING ZIF_mds_api_field=>ty_data( <ds_field> ).

      if <ds_field>-inttype is initial. "TODO - replace by new type
        field_data-is_inttype = abap_true. "TODO - replace by new type
      endif.

      IF <ds_field>-decimals <> 0.
        field_data-length_string = |{ <ds_field>-leng ALPHA = OUT },{ <ds_field>-decimals ALPHA = OUT }|.
      ELSE.
        field_data-length_string = |{ <ds_field>-leng ALPHA = OUT }|.
      ENDIF.

      IF line_exists( field_texts[ field_name = <ds_field>-field_name spras = sy-langu ] ).
        ASSIGN field_texts[ field_name = <ds_field>-field_name spras = sy-langu ] TO FIELD-SYMBOL(<field_text>).
      ELSEIF line_exists( field_texts[ field_name = <ds_field>-field_name spras = 'E' ] ).
        ASSIGN field_texts[ field_name = <ds_field>-field_name spras = sy-langu ] TO <field_text>.
      ELSE.
        UNASSIGN <field_text>.
      ENDIF.

      IF <field_text> IS ASSIGNED.
        IF     <field_text>-ddtext IS NOT INITIAL.
          field_data-description = <field_text>-ddtext.
        ELSEIF <field_text>-scrtext_l IS NOT INITIAL.
          field_data-description = <field_text>-scrtext_l.
        ELSEIF <field_text>-scrtext_m IS NOT INITIAL.
          field_data-description = <field_text>-scrtext_m.
        ELSEIF <field_text>-scrtext_s IS NOT INITIAL.
          field_data-description = <field_text>-scrtext_s.
        ELSEIF <field_text>-reptext   IS NOT INITIAL.
          field_data-description = <field_text>-reptext.
        ENDIF.

      ENDIF.

      DATA(field) = ZCL_mds_api_field=>get_instance( i_field_id =  ZCL_mds_api=>build_object_id( VALUE zmds_fd_semkey( ds_id      = me->ZIF_mds_api_datasource~header-ds_id
                                                                                                                                            field_name = <ds_field>-field_name ) )
                                                            i_data = field_data ).


      APPEND VALUE #( field_id = field->get_id( )
                      api      = field ) TO r_fields.

    ENDLOOP.

  ENDMETHOD.


  METHOD ZIF_mds_api_datasource~get_parameters.

    SELECT cdsparameter~parametername AS parameter_name,
           cdsparameter~posnr AS position,
           cdsparameter~parametername_raw AS description,
           cdsparameter~datatype,
           cdsparameter~leng AS length,
           cdsparameter~decimals,
           cdsparameter~rollname AS data_element
            FROM dd10b AS cdsparameter
           WHERE cdsparameter~strucobjn = @me->ZIF_mds_api_datasource~header-name
             AND cdsparameter~as4local  = @ZCL_mds_api_ds=>version-active
           ORDER BY position
           INTO TABLE @DATA(parameters).

    LOOP AT parameters ASSIGNING FIELD-SYMBOL(<parameter>).

      APPEND VALUE #( parameter_id = ZCL_mds_api=>build_object_id( VALUE zmds_pr_semkey( ds_id          = me->ZIF_mds_api_datasource~header-ds_id
                                                                                                       parameter_name = <parameter>-parameter_name ) )
                     ds_id         = me->ZIF_mds_api_datasource~header-ds_id )
                   TO r_parameters ASSIGNING FIELD-SYMBOL(<r_parameter>).

      <r_parameter> = CORRESPONDING #( BASE ( <r_parameter> ) <parameter> ).

    ENDLOOP.

  ENDMETHOD.


  METHOD class_constructor.

    SELECT SINGLE @abap_true AS mte
           FROM dd02l
           WHERE tabname = 'DDLX_RT_HEADER'
           INTO @DATA(metadata_extension_available).

  ENDMETHOD.


  METHOD constructor.

    super->constructor( i_sematic_key ).

    SELECT SINGLE head~strucobjn AS name,
                  head~strucobjn_raw AS cs_name,
                  head~chguser AS changed_by,
*                  head~chgdate as changed_at,
                  text~ddtext  AS description,
                  dbtabname AS sqlviewname
           FROM dd02b AS head
           INNER JOIN dd02bnd AS sqlview
                 ON  sqlview~strucobjn = head~strucobjn
                 AND sqlview~as4local  = head~as4local
           LEFT OUTER JOIN dd02bt AS text
                ON  text~strucobjn = head~strucobjn
                AND text~as4local  = head~as4local
           INTO CORRESPONDING FIELDS OF @me->ZIF_mds_api_datasource~header
           WHERE head~strucobjn = @i_sematic_key-name
             AND head~as4local  = @version-active.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_mds_id
        EXPORTING
          textid     = zcx_mds_id=>ds_not_found
          datasource = i_sematic_key-name.
    ENDIF.

    DATA(sqlview_object_id) = ZCL_mds_api=>build_object_id( VALUE zmds_ds_semkey(  type = ZIF_mds_api_datasource~type-sqlview
                                                                                                 name = me->ZIF_mds_api_datasource~header-sqlviewname ) ).

    APPEND VALUE #( link_id       = ZCL_mds_api=>build_object_id( VALUE zmds_lk_semkey( object_id1 = me->ZIF_mds_api_datasource~header-ds_id
                                                                                                      object_id2 = sqlview_object_id ) )
                    object_id1    = me->ZIF_mds_api_datasource~header-ds_id
                    object_id2    = sqlview_object_id
                    description   = relation_cust-sqlview-description
                    relation_type = relation_cust-sqlview-type ) TO me->ZIF_mds_api_datasource~relations.

  ENDMETHOD.
ENDCLASS.
