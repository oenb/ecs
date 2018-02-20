CLASS zcx_ecs_core_services DEFINITION
  PUBLIC
  INHERITING FROM cx_static_check
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_t100_message .

    CONSTANTS:
      BEGIN OF no_configuration,
        msgid TYPE symsgid VALUE 'ZECS_CORE_SERVICES',
        msgno TYPE symsgno VALUE '001',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF no_configuration .
    CONSTANTS:
      BEGIN OF error_compress_binary,
        msgid TYPE symsgid VALUE 'ZECS_CORE_SERVICES',
        msgno TYPE symsgno VALUE '002',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF error_compress_binary .
    CONSTANTS:
      BEGIN OF error_calculate_hash,
        msgid TYPE symsgid VALUE 'ZECS_CORE_SERVICES',
        msgno TYPE symsgno VALUE '003',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF error_calculate_hash .
    CONSTANTS:
      BEGIN OF error_create_by_dest,
        msgid TYPE symsgid VALUE 'ZECS_CORE_SERVICES',
        msgno TYPE symsgno VALUE '004',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF error_create_by_dest .
    CONSTANTS:
      BEGIN OF error_send,
        msgid TYPE symsgid VALUE 'ZECS_CORE_SERVICES',
        msgno TYPE symsgno VALUE '005',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF error_send .
    CONSTANTS:
      BEGIN OF error_receive,
        msgid TYPE symsgid VALUE 'ZECS_CORE_SERVICES',
        msgno TYPE symsgno VALUE '006',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF error_receive .
    CONSTANTS:
      BEGIN OF error_convert_codepage,
        msgid TYPE symsgid VALUE 'ZECS_CORE_SERVICES',
        msgno TYPE symsgno VALUE '007',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF error_convert_codepage .
    CONSTANTS:
      BEGIN OF error_http_status,
        msgid TYPE symsgid VALUE 'ZECS_CORE_SERVICES',
        msgno TYPE symsgno VALUE '008',
        attr1 TYPE scx_attrname VALUE 'MSGV1',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF error_http_status .
    CONSTANTS:
      BEGIN OF error_get_destination,
        msgid TYPE symsgid VALUE 'ZECS_CORE_SERVICES',
        msgno TYPE symsgno VALUE '009',
        attr1 TYPE scx_attrname VALUE 'MSGV1',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF error_get_destination.
    DATA msgv1 TYPE msgv1 .

    METHODS constructor
      IMPORTING
        !textid   LIKE if_t100_message=>t100key OPTIONAL
        !previous LIKE previous OPTIONAL .
    METHODS set_message_text
      IMPORTING
        !message_text TYPE string .
    METHODS get_message_text
      RETURNING
        VALUE(message_text) TYPE string .
  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA gd_message_text TYPE string .
ENDCLASS.



CLASS zcx_ecs_core_services IMPLEMENTATION.


  METHOD constructor ##ADT_SUPPRESS_GENERATION.
    CALL METHOD super->constructor
      EXPORTING
        previous = previous.
    CLEAR me->textid.
    IF textid IS INITIAL.
      if_t100_message~t100key = if_t100_message=>default_textid.
    ELSE.
      if_t100_message~t100key = textid.
    ENDIF.
  ENDMETHOD.


  METHOD get_message_text.
    message_text = gd_message_text.
  ENDMETHOD.


  METHOD set_message_text.
    gd_message_text = message_text.
  ENDMETHOD.
ENDCLASS.
