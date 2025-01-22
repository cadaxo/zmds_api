CLASS ZCL_mds_api_ds_tabl DEFINITION INHERITING FROM ZCL_mds_api_ds
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS constructor IMPORTING i_sematic_key TYPE zmds_ds_semkey.
    METHODS ZIF_mds_api_datasource~build_related_entities REDEFINITION.
    METHODS ZIF_mds_api_datasource~get_fields REDEFINITION.
    METHODS ZIF_mds_api_datasource~get_action_links REDEFINITION.

ENDCLASS.


CLASS ZCL_mds_api_ds_tabl IMPLEMENTATION.

  METHOD constructor.

    super->constructor( i_sematic_key ).

    SELECT SINGLE head~tabname  AS name,
                  head~as4user AS changed_by,
                  text~ddtext  AS description
                  FROM dd02l AS head
                  LEFT OUTER JOIN dd02t AS text
                    ON  text~tabname  = head~tabname
                    AND text~as4local = head~as4local
                  INTO CORRESPONDING FIELDS OF @me->ZIF_mds_api_datasource~header
                  WHERE head~tabname  = @i_sematic_key-name
                    AND head~as4local = @version-active.
    IF sy-subrc <> 0.
      MESSAGE '' TYPE 'X'.
    ENDIF.

  ENDMETHOD.


  METHOD ZIF_mds_api_datasource~build_related_entities.

    IF me->related_read = abap_false.

      IF me->ZIF_mds_api_datasource~header-role >= ZIF_mds_api=>ds_role-main.
        SELECT b~strucobjn
               FROM dd26s AS a
               INNER JOIN dd02bnd AS b
                     ON b~dbtabname = a~viewname
               WHERE a~tabname  = @ZIF_mds_api_datasource~header-name
                 AND b~as4local = @version-active
               INTO TABLE @DATA(base_tables).

        LOOP AT base_tables ASSIGNING FIELD-SYMBOL(<base_table>).

          APPEND VALUE #( link_id       = 'GET_ID'
                          object_id1    = me->ZIF_mds_api_datasource~header-ds_id
                          object_id2    = ZCL_mds_api=>build_object_id( VALUE zmds_ds_semkey(  type = ZIF_mds_api_datasource~type-datadefinition
                                                                                                             name = <base_table>-strucobjn ) )
                          description   = relation_cust-isused-description
                          relation_type = relation_cust-isused-type ) TO me->ZIF_mds_api_datasource~relations.

        ENDLOOP.

        LOOP AT me->ZIF_mds_api_datasource~relations ASSIGNING FIELD-SYMBOL(<relation>) WHERE link_id = 'GET_ID'.
          <relation>-link_id = ZCL_mds_api=>build_object_id( <relation>-semkey ).
        ENDLOOP.

      ENDIF.

      me->related_read = abap_true.

    ENDIF.

  ENDMETHOD.

  METHOD ZIF_mds_api_datasource~get_fields.

    SELECT fields~fieldname AS field_name,
           fields~keyflag AS is_key,
           fields~position,
           fields~datatype,
           fields~leng,
           fields~decimals,
           fields~rollname AS data_element
           FROM dd03l AS fields
           WHERE tabname    = @me->ZIF_mds_api_datasource~header-name
             AND as4local   = @version-active
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
           WHERE tabname    = @me->ZIF_mds_api_datasource~header-name
             AND domstat    = @version-active
             AND textstat   = @version-active
             AND ( ddlanguage = @sy-langu OR ddlanguage = 'E' )
           INTO TABLE @DATA(field_texts).

    me->ds_fields = CORRESPONDING #( fields ).

    LOOP AT fields ASSIGNING FIELD-SYMBOL(<ds_field>) WHERE field_name NP '.INC*'.

      DATA(field_data) = CORRESPONDING ZIF_mds_api_field=>ty_data( <ds_field> ).
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

* UI Shows only field_alias?!?
*        IF <field_text>-scrtext_l IS NOT INITIAL.
*          field_data-field_alias = <field_text>-scrtext_l.
*        ELSEIF <field_text>-scrtext_m IS NOT INITIAL.
*          field_data-field_alias = <field_text>-scrtext_m.
*        ELSEIF <field_text>-scrtext_s IS NOT INITIAL.
*          field_data-field_alias = <field_text>-scrtext_s.
*        ELSEIF <field_text>-ddtext IS NOT INITIAL.
*          field_data-field_alias = <field_text>-ddtext.
*        ELSEIF <field_text>-reptext   IS NOT INITIAL.
*          field_data-field_alias = <field_text>-reptext.
*        ENDIF.
      ENDIF.
      field_data-field_alias = <ds_field>-field_name.

      DATA(field) = ZCL_mds_api_field=>get_instance( i_field_id =  ZCL_mds_api=>build_object_id( VALUE zmds_fd_semkey( ds_id      = me->ZIF_mds_api_datasource~header-ds_id
                                                                                                                                            field_name = <ds_field>-field_name ) )
                                                            i_data = field_data ).

      APPEND VALUE #( field_id = field->get_id( )
                      api      = field ) TO r_fields.

    ENDLOOP.

  ENDMETHOD.

  METHOD ZIF_mds_api_datasource~get_action_links.

    r_links_action = super->ZIF_mds_api_datasource~get_action_links( ).

    r_links_action-display = |/sap/bc/adt/ddic/structures/{ me->ZIF_mds_api_datasource~header-name }/source/main?version=active&sap-client={ sy-mandt }|.

    IF r_links_action-edit IS INITIAL.
      r_links_action-edit = |adt://{ sy-sysid }/sap/bc/adt/vit/wb/object_type/tabldt/object_name/{ me->ZIF_mds_api_datasource~header-name }|.
    ENDIF.

  ENDMETHOD.

ENDCLASS.
