CLASS ZCL_mds_api_field DEFINITION
  PUBLIC
  CREATE PROTECTED.

  PUBLIC SECTION.
    INTERFACES ZIF_mds_api_field.

    CLASS-METHODS get_instance IMPORTING i_field_id        TYPE zmds_field_id
                                         i_data            TYPE ZIF_mds_api_field~ty_data OPTIONAL
                               RETURNING VALUE(e_instance) TYPE REF TO ZIF_mds_api_field .
    METHODS constructor IMPORTING i_field_id TYPE zmds_field_id.

protected section.
private section.
ENDCLASS.



CLASS ZCL_MDS_API_FIELD IMPLEMENTATION.
ENDCLASS.
