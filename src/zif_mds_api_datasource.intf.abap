INTERFACE ZIF_mds_api_datasource
  PUBLIC .
  CONSTANTS: BEGIN OF type,
               datadefinition    TYPE string VALUE 'DDLS',
               enhancment        TYPE string VALUE 'YDLS',
               table             TYPE string VALUE 'TABL',
               sqlview           TYPE string VALUE 'YABL',
               metadataextension TYPE string VALUE 'DDLX',
             END OF type,
             BEGIN OF relation_cust,
                          BEGIN OF base,
                            type        TYPE string VALUE 'BASE',
                            description TYPE string VALUE 'as select from',
                            role        TYPE ZIF_mds_api=>ty_ds_role VALUE ZIF_mds_api=>ds_role-parent,
                          END OF base,
                          BEGIN OF enhancement,
                            type        TYPE string VALUE 'ENHANCEMENT',
                            description TYPE string VALUE 'enhances',
                            role        TYPE ZIF_mds_api=>ty_ds_role VALUE ZIF_mds_api=>ds_role-parent,
                          END OF enhancement,
                          BEGIN OF isused,
                            type        TYPE string VALUE 'ISUSED',
                            description TYPE string VALUE 'is used in',
                            role        TYPE ZIF_mds_api=>ty_ds_role VALUE ZIF_mds_api=>ds_role-child,
                          END OF isused,
                          BEGIN OF sqlview,
                            type        TYPE string VALUE 'SQLVIEW',
                            description TYPE string VALUE 'has SQL View',
                            role        TYPE ZIF_mds_api=>ty_ds_role VALUE ZIF_mds_api=>ds_role-parent,
                          END OF sqlview,
                          BEGIN OF metaextension,
                            type        TYPE string VALUE 'METADATAEXTENSION',
                            description TYPE string VALUE 'has Metadata Extension',
                            role        TYPE ZIF_mds_api=>ty_ds_role VALUE ZIF_mds_api=>ds_role-parent,
                          END OF metaextension,
                        END OF relation_cust.

  METHODS build_related_entities.
  METHODS get_relations RETURNING VALUE(r_relations) TYPE ZIF_mds_api=>ty_relations.
  METHODS get_datasource RETURNING VALUE(r_datasource) TYPE ZIF_mds_api=>ty_datasource.
  METHODS get_fields RETURNING VALUE(r_fields) TYPE ZIF_mds_api=>ty_fields.
  METHODS get_annotations RETURNING VALUE(r_annotations) TYPE ZIF_mds_api=>ty_annotations.
  METHODS get_parameters RETURNING VALUE(r_parameters) TYPE ZIF_mds_api=>ty_parameters.
  METHODS get_properties RETURNING VALUE(r_properties) TYPE ZIF_mds_api=>ty_properties.
  METHODS get_action_links RETURNING VALUE(r_links_action) TYPE ZIF_mds_api=>ty_action_link.
  METHODS set_role IMPORTING i_role TYPE ZIF_mds_api=>ty_ds_role.
  METHODS has_field IMPORTING i_fieldname_search       TYPE zmds_field_search
                    RETURNING VALUE(r_field_source_ds) TYPE zmds_field_search.
  METHODS uses_field IMPORTING i_fieldname_search       TYPE zmds_field_search
                     RETURNING VALUE(r_field_source_ds) TYPE zmds_field_search.
  DATA header TYPE ZIF_mds_api=>ty_datasource READ-ONLY.
  DATA relations TYPE ZIF_mds_api=>ty_relations READ-ONLY.
  DATA fields TYPE ZIF_mds_api=>ty_fields READ-ONLY.
  DATA annotations TYPE ZIF_mds_api=>ty_annotations READ-ONLY.
  DATA parameters TYPE ZIF_mds_api=>ty_parameters READ-ONLY.

ENDINTERFACE.
