class ZCL_ZOV_MPC_EXT definition
  public
  inheriting from ZCL_ZOV_MPC
  create public .

public section.

  types:
    BEGIN OF ty_ordem_item,
        OrdemId     TYPE i,
        DataCriacao TYPE timestamp,
        CriadoPor   TYPE c LENGTH 20,
        ClienteId   TYPE i,
        TotalItens  TYPE p LENGTH 8 DECIMALS 2,
        TotalFrete  TYPE p LENGTH 8 DECIMALS 2,
        TotalOrdem  TYPE p LENGTH 8 DECIMALS 2,
        Status      TYPE c LENGTH 1,
        toOVItem    TYPE TABLE OF ts_ovitem WITH DEFAULT KEY,
      END OF ty_ordem_item .

  methods DEFINE
    redefinition .
protected section.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_ZOV_MPC_EXT IMPLEMENTATION.


  method DEFINE.
    DATA lo_entity_type TYPE REF TO /iwbep/if_mgw_odata_entity_typ.

    super->define( ).

    lo_entity_type = model->get_entity_type( iv_entity_name = 'OVCab' ).
    lo_entity_type->bind_structure( iv_structure_name = 'ZCL_ZOV_MPC_EXT=>TY_ORDEM_ITEM' ).

  endmethod.
ENDCLASS.
