class ZCL_ZOV_DPC_EXT definition
  public
  inheriting from ZCL_ZOV_DPC
  create public .

public section.
protected section.

  methods MENSAGEMSET_CREATE_ENTITY
    redefinition .
  methods MENSAGEMSET_DELETE_ENTITY
    redefinition .
  methods MENSAGEMSET_GET_ENTITY
    redefinition .
  methods MENSAGEMSET_GET_ENTITYSET
    redefinition .
  methods MENSAGEMSET_UPDATE_ENTITY
    redefinition .
  methods OVCABSET_CREATE_ENTITY
    redefinition .
  methods OVCABSET_DELETE_ENTITY
    redefinition .
  methods OVCABSET_GET_ENTITY
    redefinition .
  methods OVCABSET_GET_ENTITYSET
    redefinition .
  methods OVCABSET_UPDATE_ENTITY
    redefinition .
  methods OVITEMSET_CREATE_ENTITY
    redefinition .
  methods OVITEMSET_DELETE_ENTITY
    redefinition .
  methods OVITEMSET_GET_ENTITY
    redefinition .
  methods OVITEMSET_GET_ENTITYSET
    redefinition .
  methods OVITEMSET_UPDATE_ENTITY
    redefinition .
private section.
ENDCLASS.



CLASS ZCL_ZOV_DPC_EXT IMPLEMENTATION.


  method MENSAGEMSET_CREATE_ENTITY.
  endmethod.


  method MENSAGEMSET_DELETE_ENTITY.
  endmethod.


  method MENSAGEMSET_GET_ENTITY.
  endmethod.


  method MENSAGEMSET_GET_ENTITYSET.
  endmethod.


  method MENSAGEMSET_UPDATE_ENTITY.
  endmethod.


  method OVCABSET_CREATE_ENTITY.
    DATA: ld_lastid TYPE int4.
    DATA: ls_cab TYPE zovcab.

    DATA(lo_msg) = me->/iwbep/if_mgw_conv_srv_runtime~get_message_container( ).

    io_data_provider->read_entry_data(
      IMPORTING
        es_data = er_entity
    ).

    MOVE-CORRESPONDING er_entity to ls_cab.

    ls_cab-criacao_data = sy-datum.
    ls_cab-criacao_hora = sy-uzeit.
    ls_cab-criacao_usuario = sy-uname.

    SELECT SINGLE MAX( ordemid )
      INTO ld_lastid
      FROM zovcab.

    ls_cab-ordemid = ld_lastid + 1.
    INSERT zovcab FROM ls_cab.
    IF sy-subrc <> 0.
      lo_msg->add_message_text_only(
        EXPORTING
          iv_msg_type = 'E'
          iv_msg_text = 'Erro ao inserir ordem'
      ).

      RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
        EXPORTING
          message_container = lo_msg.
    ENDIF.

    MOVE-CORRESPONDING ls_cab to  er_entity.

    CONVERT
      date  ls_cab-criacao_data
      TIME ls_cab-criacao_hora
      into TIME STAMP er_entity-datacriacao
      TIME ZONE sy-zonlo.
  endmethod.


  method OVCABSET_DELETE_ENTITY.
  endmethod.


  method OVCABSET_GET_ENTITY.
    er_entity-ordemid = 1.
    er_entity-criadopor = 'Marcos'.
    er_entity-datacriacao = '19700101000000'.
  endmethod.


  method OVCABSET_GET_ENTITYSET.
    DATA: lt_cab type STANDARD TABLE OF zovcab.
    DATA: ls_cab TYPE zovcab.
    DATA: ls_entityset LIKE LINE OF et_entityset.

    select *
      INTO TABLE lt_cab
      FROM zovcab.

   LOOP AT lt_cab INTO ls_cab.
     clear ls_entityset.
     MOVE-CORRESPONDING ls_cab to ls_entityset.

     ls_entityset-criadopor = ls_cab-criacao_usuario.

     CONVERT  DATE ls_cab-criacao_data
              TIME ls_cab-criacao_hora
           into TIME STAMP ls_entityset-datacriacao
           TIME ZONE sy-zonlo.

     APPEND ls_entityset to et_entityset.
   ENDLOOP.
  endmethod.


  method OVCABSET_UPDATE_ENTITY.
  endmethod.


  METHOD ovitemset_create_entity.
    DATA: ls_item TYPE zovitem.

    DATA(lo_msg) = me->/iwbep/if_mgw_conv_srv_runtime~get_message_container( ).

    io_data_provider->read_entry_data(
      IMPORTING
        es_data = er_entity
    ).

    MOVE-CORRESPONDING er_entity TO ls_item.

    IF er_entity-itemid = 0.
      SELECT SINGLE MAX( itemid )
        INTO er_entity-itemid
        FROM zovitem
        WHERE ordemid = er_entity-ordemid.

      er_entity-itemid = er_entity-itemid + 1.
    ENDIF.

    INSERT zovitem FROM ls_item.
    IF sy-subrc <> 0.
      lo_msg->add_message_text_only(
      EXPORTING
        iv_msg_type  = 'E'
        iv_msg_text = 'Erro ao inserir item'
      ).

      RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
        EXPORTING
          message_container = lo_msg.
    ENDIF.
  ENDMETHOD.


  method OVITEMSET_DELETE_ENTITY.
  endmethod.


  method OVITEMSET_GET_ENTITY.
  endmethod.


  method OVITEMSET_GET_ENTITYSET.
    DATA: ld_ordemid type int4.
    DATA: lt_ordemid_range TYPE RANGE OF int4.
    DATA: ls_ordemid_range LIKE LINE OF lt_ordemid_range.
    DATA: ls_key_tab LIKE LINE OF it_key_tab.

    READ TABLE it_key_tab INTO ls_key_tab with KEY name = 'OrdemId'.
    IF sy-subrc = 0.
      ld_ordemid = ls_key_tab-value.

      clear ls_ordemid_range.
      ls_ordemid_range-sign = 'I'.
      ls_ordemid_range-option = 'EQ'.
      ls_ordemid_range-low = ld_ordemid.
      APPEND ls_ordemid_range to lt_ordemid_range.
    ENDIF.

    select *
      INTO CORRESPONDING FIELDS OF TABLE et_entityset
      FROM zovitem
      WHERE ordemid In  lt_ordemid_range.
  endmethod.


  method OVITEMSET_UPDATE_ENTITY.
  endmethod.
ENDCLASS.
