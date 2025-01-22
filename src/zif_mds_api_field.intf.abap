INTERFACE ZIF_mds_api_field
  PUBLIC .

  TYPES: BEGIN OF ty_data,
           field_alias            TYPE fieldname,
           "field_type             type zmds_field_type,
           is_key                 TYPE zmds_is_key,
           is_inttype             TYPE zmds_is_inttype,
           base_table             TYPE tabname,
           base_field_name        TYPE fieldname,
           origin_field_name      TYPE ddfldorigin,
           origin_appendstru_name TYPE appname,
           position               TYPE ddfdpos,
           description            TYPE string,
           object_state           TYPE int4,
           datatype               TYPE datatype_d,
           length                 TYPE ddleng,
           decimals               TYPE decimals,
           data_element           TYPE rollname,
           length_string          TYPE string,
         END OF ty_data.

  TYPES: BEGIN OF ty_field.
  INCLUDE TYPE zmds_fd_semkey AS semkey.
  INCLUDE TYPE ZIF_mds_api_field=>ty_data AS data.
  TYPES: field_id TYPE zmds_field_id,
         END OF ty_field.

  METHODS get_id RETURNING VALUE(e_field_id) TYPE zmds_field_id.
  METHODS get_semantic_key RETURNING VALUE(e_semantic_key) TYPE zmds_fd_semkey.
  METHODS get_as_structure RETURNING VALUE(e_field) TYPE ty_field.

ENDINTERFACE.
