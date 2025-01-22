CLASS ZCL_mds_api_ds_ddlx DEFINITION INHERITING FROM ZCL_mds_api_ds
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS constructor IMPORTING i_sematic_key TYPE zmds_ds_semkey.

ENDCLASS.


CLASS ZCL_mds_api_ds_ddlx IMPLEMENTATION.

  METHOD constructor.

    super->constructor( i_sematic_key ).

    SELECT SINGLE header~ddlxname AS name,
                  source~changed_by AS changed_by,
                  text~description AS description
           FROM ddlx_rt_header AS header
           INNER JOIN ddlxsrc AS source
            ON  source~uuid     = header~dt_uuid
            AND source~ddlxname = header~ddlxname
           LEFT OUTER JOIN ddlxsrct AS text
            ON  text~ddlxname = header~ddlxname
            AND text~version  = source~version
            AND text~language = @sy-langu
        INTO CORRESPONDING FIELDS OF @me->ZIF_mds_api_datasource~header
        WHERE header~ddlxname = @i_sematic_key-name
          AND source~version = @version-active.

    IF sy-subrc <> 0.
      MESSAGE '' TYPE 'X'.
    ENDIF.

  ENDMETHOD.


ENDCLASS.
