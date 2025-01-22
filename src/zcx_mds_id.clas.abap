class zCX_MDS_ID definition
  public
  inheriting from CX_STATIC_CHECK
  create public .

public section.

  interfaces IF_T100_MESSAGE .

  constants:
    begin of DS_NOT_FOUND,
      msgid type symsgid value 'ZMDS',
      msgno type symsgno value '001',
      attr1 type scx_attrname value 'DATASOURCE',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of DS_NOT_FOUND .
  data DATASOURCE type ZMDS_OBJECT_NAME .

  methods CONSTRUCTOR
    importing
      !TEXTID like IF_T100_MESSAGE=>T100KEY optional
      !PREVIOUS like PREVIOUS optional
      !DATASOURCE type ZMDS_OBJECT_NAME optional .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zCX_MDS_ID IMPLEMENTATION.


  method CONSTRUCTOR.
CALL METHOD SUPER->CONSTRUCTOR
EXPORTING
PREVIOUS = PREVIOUS
.
me->DATASOURCE = DATASOURCE .
clear me->textid.
if textid is initial.
  IF_T100_MESSAGE~T100KEY = IF_T100_MESSAGE=>DEFAULT_TEXTID.
else.
  IF_T100_MESSAGE~T100KEY = TEXTID.
endif.
  endmethod.
ENDCLASS.
