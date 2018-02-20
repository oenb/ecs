CLASS zcl_ecs_core_services DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .


  PUBLIC SECTION.
    INTERFACES zif_ecs_core_services.
    ALIASES:
      put_object          FOR zif_ecs_core_services~put_object,
      list_bucket_objects FOR zif_ecs_core_services~list_bucket_objects,
      get_object          FOR zif_ecs_core_services~get_object.

    "! Factory method
    "! @parameter i_bucket | Bucket
    "! @parameter r_result | Reference to Core Services
    CLASS-METHODS factory
      IMPORTING i_bucket        TYPE zecs_bucket
                i_debug         TYPE abap_bool
      RETURNING VALUE(r_result) TYPE REF TO zcl_ecs_core_services
      RAISING   zcx_ecs_core_services.


  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA:
      gs_ecs_config TYPE zecs_config,
      gd_bucket     TYPE zecs_bucket,
      gd_debug      TYPE abap_bool.

    "! Get configuration from customizing
    "! @parameter r_result | ECS Configuration
    METHODS get_config
      RETURNING VALUE(r_result) TYPE zecs_config
      RAISING   zcx_ecs_core_services.

    "! Content encryption
    "! @parameter i_content_bin | Binary file
    "! @parameter r_result | Encrypted binary file
    METHODS encrypt_content_bin
      IMPORTING i_content_bin   TYPE xstring
      RETURNING VALUE(r_result) TYPE xstring.

    "! Encrypt the file using SSF(Secure Store and Forward)
    "! @parameter i_content_bin | Binary file
    "! @parameter e_length | File length
    "! @parameter e_ssfbin | SSF binary data
    METHODS xstring_to_ssfbin
      IMPORTING i_content_bin TYPE xstring
      EXPORTING e_length      TYPE ssflen
                e_ssfbin      TYPE zecs_t_ssfbin.

    "! Get timestamp in ISO8601 basic format e.g. 20171229T135959Z
    "! @parameter r_result | Timestamp
    METHODS get_tstmp_iso8601basicformat
      RETURNING VALUE(r_result) TYPE string.

    "! Canonicalize request header names
    "! @parameter it_included_headernames | Header names
    "! @parameter r_canonicalizedheadernames | Canonicalized header names string
    METHODS get_canonicalizeheadernames
      IMPORTING it_included_headernames           TYPE tihttpnvp
      RETURNING VALUE(r_canonicalizedheadernames) TYPE string.

    "! Canonicalize request header
    "! @parameter it_included_headernames | Header names
    "! @parameter r_canonicalizedheaders | Canonicalized headers string
    METHODS get_canonicalizedheaderstring
      IMPORTING it_included_headernames       TYPE tihttpnvp
      RETURNING VALUE(r_canonicalizedheaders) TYPE string.

    "! Header names in use
    "! @parameter ir_client | Client
    "! @parameter rt_included_headernames | Used header names
    METHODS get_included_headernames
      IMPORTING ir_client                      TYPE REF TO if_http_client
      RETURNING VALUE(rt_included_headernames) TYPE tihttpnvp.

    "! Hash signature character string
    "! @parameter i_string | Character string to be hashed
    "! @parameter i_key | Hash key
    "! @parameter r_result | Hashed string
    "! @raising zcx_ecs_core_services | Exception
    METHODS hmac_sign_kstring
      IMPORTING i_string        TYPE string
                i_key           TYPE string
      RETURNING VALUE(r_result) TYPE xstring
      RAISING   zcx_ecs_core_services.

    "! Hash signature hex string
    "! @parameter i_string | Character string to be hashed
    "! @parameter i_xkey | Hex hash key
    "! @parameter r_result | Hashed hex string
    METHODS hmac_sign_kxstring
      IMPORTING i_string        TYPE string
                i_xkey          TYPE xstring
      RETURNING VALUE(r_result) TYPE xstring.

    "! Set content type header field
    "! @parameter i_content_type | HTTP Method GET, PUT, DELETE, ...
    "! @parameter rt_request_headers | Request header table
    METHODS set_content_type
      IMPORTING i_content_type            TYPE string
      RETURNING VALUE(rt_request_headers) TYPE tihttpnvp.

    "! Create HTTP client from RFC destination
    "! @parameter r_client | Client
    "! @raising zcx_ecs_core_services | Exception
    METHODS create_client
      RETURNING VALUE(r_client) TYPE REF TO if_http_client
      RAISING   zcx_ecs_core_services.

    "! Get server name (inclusive port) from RFC destination
    "! @parameter r_host_name | Host name and port
    "! @raising zcx_ecs_core_services | Exception
    METHODS get_server_name_and_port
      RETURNING VALUE(r_host_name) TYPE string
      RAISING   zcx_ecs_core_services.

    "! Set additional header fields
    "! @parameter it_request_headers | Request header table
    "! @parameter ir_client | Client
    METHODS set_additional_header_fields
      IMPORTING it_request_headers TYPE tihttpnvp
                ir_client          TYPE REF TO if_http_client.

    "! Sign request with AWS Signature Version 4
    "! @parameter i_host_name | Host name
    "! @parameter ir_client | Client
    "! @parameter i_http_method | HTTP Method GET, PUT, DELETE, ...
    "! @raising zcx_ecs_core_services | Exception
    METHODS aws_signature_v4
      IMPORTING i_host_name   TYPE string
                i_object_name TYPE string OPTIONAL
                i_query_para  TYPE string OPTIONAL
                i_http_method TYPE string
                i_content_bin TYPE xstring OPTIONAL
                ir_client     TYPE REF TO if_http_client
      RAISING   zcx_ecs_core_services.

    "! Send Request to Server
    "! @parameter ir_client | Client
    "! @raising zcx_ecs_core_services | Exception
    METHODS send_to_server
      IMPORTING ir_client TYPE REF TO if_http_client
      RAISING   zcx_ecs_core_services.

    "! Receive Request from Server
    "! @parameter ir_client | Client
    "! @raising zcx_ecs_core_services | Exception
    METHODS receive_from_server
      IMPORTING ir_client TYPE REF TO if_http_client
      RAISING   zcx_ecs_core_services.

    "! Get Response Header Fields
    "! @parameter ir_client | Client
    "! @parameter et_response_headers | Response headers
    "! @raising zcx_ecs_core_services | Exception
    METHODS get_response_header_fields
      IMPORTING ir_client                  TYPE REF TO if_http_client
      RETURNING VALUE(et_response_headers) TYPE tihttpnvp
      RAISING   zcx_ecs_core_services.

    "! Get Response Content as String
    "! @parameter ir_client | Client
    "! @parameter r_response_content | Response String
    "! @raising zcx_ecs_core_services | Exception
    METHODS get_response_content
      IMPORTING ir_client                 TYPE REF TO if_http_client
      RETURNING VALUE(r_response_content) TYPE string
      RAISING   zcx_ecs_core_services.
ENDCLASS.


CLASS zcl_ecs_core_services IMPLEMENTATION.

  METHOD factory.
* importing i_bucket  type zecs_bucket
*           i_debug  type abap_bool
* returning value(r_result)  type ref to zcl_ecs_core_services
* raising zcx_ecs_core_services

    r_result = NEW zcl_ecs_core_services( ).
    r_result->gd_bucket = i_bucket.
    r_result->gd_debug = i_debug.
    r_result->gs_ecs_config = r_result->get_config( ).
  ENDMETHOD.


  METHOD get_config.
* returning value(r_result)  type zecs_config
* raising zcx_ecs_core_services

    SELECT SINGLE *
      FROM zecs_config
      INTO @r_result
      WHERE bucket = @gd_bucket.

    IF sy-subrc IS NOT INITIAL.
      RAISE EXCEPTION TYPE zcx_ecs_core_services
        EXPORTING
          textid = zcx_ecs_core_services=>no_configuration.
*   Fehler beim Lesen der Konfiguration
    ENDIF.
  ENDMETHOD.


  METHOD put_object.
* importing i_object_name  type string
*           i_content_bin  type xstring
* exporting et_response_headers  type tihttpnvp
*           e_response_content  type string
* raising zcx_ecs_core_services

    DATA:
      ld_content_bin    TYPE xstring,
      ld_content_length TYPE string,
      ld_uri            TYPE string,
      ld_host_name      TYPE string.

* ECS Header data
    DATA(lt_request_headers) = set_content_type( zif_ecs_core_services=>c_content_type ).

* Server side encryption
    IF gs_ecs_config-server_encrypt = abap_true.
      lt_request_headers = VALUE #( BASE lt_request_headers
                                    ( name = 'x-amz-server-side-encryption'
                                      value = 'AES256' ) ).
      IF gd_debug = abap_true.
        WRITE: / |{ lt_request_headers[ name = 'x-amz-server-side-encryption' ]-name }: { lt_request_headers[ name = 'x-amz-server-side-encryption' ]-value }|.
      ENDIF.
    ENDIF.

* Compression
    IF gs_ecs_config-zip = abap_true.
      TRY.
          cl_abap_gzip=>compress_binary( EXPORTING raw_in   = i_content_bin
                                         IMPORTING gzip_out = ld_content_bin ).

        CATCH cx_parameter_invalid_range    " Parameter with invalid value range
              cx_sy_buffer_overflow    " System-Exception: buffer too short
              cx_sy_compression_error.    " System-Exception: compression error

          RAISE EXCEPTION TYPE zcx_ecs_core_services
            EXPORTING
              textid = zcx_ecs_core_services=>error_compress_binary.
*   Error during compression (ZIP)
      ENDTRY.
    ELSE.
      ld_content_bin = i_content_bin.
    ENDIF.

* Client side encryption
    IF gs_ecs_config-client_encrypt = abap_true.
      ld_content_bin = encrypt_content_bin( ld_content_bin ).
    ENDIF.

* Create Rest Client
    DATA(lr_client) = create_client( ).

* Set HTTP Method
    DATA(ld_http_method) = zif_ecs_core_services=>c_http_method_put.
    lr_client->request->set_method( ld_http_method ).

* Get server name and port from RFC destination
    ld_host_name = get_server_name_and_port( ).

* Set URI
    ld_uri = |/{ gs_ecs_config-bucket }/{ i_object_name }|.

    IF gd_debug = abap_true.
      WRITE: / |URI: { ld_uri }|.
    ENDIF.

    cl_http_utility=>set_request_uri( request = lr_client->request
                                      uri     = ld_uri ).

* Set additional header fields
    set_additional_header_fields( it_request_headers = lt_request_headers
                                  ir_client          = lr_client ).

* AWS Signature Version 4
    aws_signature_v4( i_host_name   = ld_host_name  "Throws exception
                      i_object_name = i_object_name
                      i_http_method = ld_http_method
                      i_content_bin = ld_content_bin
                      ir_client     = lr_client ).

* Sets data
    lr_client->request->set_data( ld_content_bin ).

* Header Content-Length
    ld_content_length = xstrlen( ld_content_bin ).
    lr_client->request->set_header_field( name  = 'Content-Length' "#EC NOTEXT
                                          value = ld_content_length ).

    IF gd_debug = abap_true.
      WRITE: / |Content-Length: { ld_content_length }|.
    ENDIF.

* Send to server
    send_to_server( lr_client ).  "Throws exception

* Receive Response
    receive_from_server( lr_client ).  "Throws exception

* Returns response header fields
    TRY.
        et_response_headers = get_response_header_fields( lr_client ).  "Throws exception
      CATCH zcx_ecs_core_services.
    ENDTRY.

* Returns response content
    e_response_content = get_response_content( lr_client ).  "Throws exception
  ENDMETHOD.


  METHOD set_content_type.
* importing i_content_type  type string
* returning value(rt_request_headers)  type tihttpnvp

    rt_request_headers  = VALUE tihttpnvp( ( name = 'Content-Type'
                                             value = i_content_type ) ) ##NO_TEXT.

    IF gd_debug = abap_true.
      WRITE: / |{ rt_request_headers[ name = 'Content-Type' ]-name }: { rt_request_headers[ name = 'Content-Type' ]-value }|.
    ENDIF.
  ENDMETHOD.


  METHOD get_canonicalizeheadernames.
* importing it_included_headernames  type tihttpnvp
* returning value(r_canonicalizedheadernames)  type string

    LOOP AT it_included_headernames  ASSIGNING FIELD-SYMBOL(<ls_ihttpnvp>).
      IF sy-tabix = 1.
        r_canonicalizedheadernames = <ls_ihttpnvp>-name.
      ELSE.
        r_canonicalizedheadernames = |{ r_canonicalizedheadernames };{ <ls_ihttpnvp>-name }|.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  METHOD encrypt_content_bin.
* importing i_content_bin  type xstring
* returning value(r_result)  type xstring

    xstring_to_ssfbin( EXPORTING i_content_bin = i_content_bin
                       IMPORTING e_length = DATA(ld_ostr_input_data_l)
                                 e_ssfbin = DATA(lt_ostr_input_data) ).

* TBD: SSFA, STRUST for client side encryption
  ENDMETHOD.


  METHOD xstring_to_ssfbin.
* importing i_content_bin  type xstring
* exporting e_length  type ssflen
*           e_ssfbin  type zecs_t_ssfbin

    DATA(ld_content_bin) = i_content_bin.
    e_length = xstrlen( i_content_bin ).

    CLEAR e_ssfbin.
    WHILE ld_content_bin IS NOT INITIAL.
      e_ssfbin = VALUE #( BASE e_ssfbin
                          ( bindata = ld_content_bin ) ).
      SHIFT ld_content_bin BY 255 PLACES LEFT IN BYTE MODE.
    ENDWHILE.
  ENDMETHOD.


  METHOD get_tstmp_iso8601basicformat.
* returning value(r_result)  type string

    DATA:
      ld_time_zone TYPE timezone.

    GET TIME STAMP FIELD DATA(ld_timestamp).

    ld_time_zone = 'UTC'.
    CONVERT TIME STAMP ld_timestamp
            TIME ZONE ld_time_zone
            INTO DATE DATA(ld_date_utc) TIME DATA(ld_time_utc).

    r_result = |{ ld_date_utc }T{ ld_time_utc }Z|.
  ENDMETHOD.


  METHOD get_canonicalizedheaderstring.
* importing it_included_headernames  type tihttpnvp
* returning value(r_canonicalizedheaders)  type string

    LOOP AT it_included_headernames ASSIGNING FIELD-SYMBOL(<ls_ihttpnvp>).
      r_canonicalizedheaders = |{ r_canonicalizedheaders }{ <ls_ihttpnvp>-name }:{ condense( <ls_ihttpnvp>-value ) }{ cl_abap_char_utilities=>newline }|.
    ENDLOOP.
  ENDMETHOD.


  METHOD get_included_headernames.
* importing ir_client  type ref to if_http_client
* returning value(rt_included_headernames)  type tihttpnvp

    ir_client->request->get_header_fields( CHANGING fields = rt_included_headernames ).
* Convert to lowercase the header names and remove unwanted headers
    LOOP AT rt_included_headernames ASSIGNING FIELD-SYMBOL(<ls_ihttpnvp>).
      IF <ls_ihttpnvp>-name = 'host' OR
         <ls_ihttpnvp>-name = 'content-type' OR
         <ls_ihttpnvp>-name = 'content-md5' OR
         <ls_ihttpnvp>-name(6) = 'x-amz-'.
        <ls_ihttpnvp>-name = to_lower( <ls_ihttpnvp>-name ).
      ELSE.
        DELETE rt_included_headernames.
      ENDIF.
    ENDLOOP.

    SORT rt_included_headernames BY name ASCENDING.
  ENDMETHOD.


  METHOD hmac_sign_kstring.
* importing i_string  type string
*           i_key  type string
* returning value(r_result)  type xstring
* raising zcx_ecs_core_services

    DATA:
      ld_xkey TYPE xstring.

    TRY.
        ld_xkey = cl_abap_hmac=>string_to_xstring( i_key ).
      CATCH cx_abap_message_digest.    " Exception class for Message Digest
        RAISE EXCEPTION TYPE zcx_ecs_core_services
          EXPORTING
            textid = zcx_ecs_core_services=>error_calculate_hash.
*   Error calculate hash
    ENDTRY.

    TRY.
        cl_abap_hmac=>calculate_hmac_for_char( EXPORTING if_algorithm   = zif_ecs_core_services=>c_algorithm    " Hash-Algorithm
                                                         if_key         = ld_xkey    " HMAC Key
                                                         if_data        = i_string    " Data
                                               IMPORTING ef_hmacxstring = r_result ).   " HMAC-Value binary as XString
      CATCH cx_abap_message_digest.    "Exception class for Message Digest
        RAISE EXCEPTION TYPE zcx_ecs_core_services
          EXPORTING
            textid = zcx_ecs_core_services=>error_calculate_hash.
*   Error calculate hash
    ENDTRY.
  ENDMETHOD.


  METHOD hmac_sign_kxstring.
* importing i_string  type string
*           i_xkey  type xstring
* returning value(r_result)  type xstring

    TRY.
        cl_abap_hmac=>calculate_hmac_for_char( EXPORTING if_algorithm   = zif_ecs_core_services=>c_algorithm    " Hash-Algorithm
                                                         if_key         = i_xkey    " HMAC Key
                                                         if_data        = i_string    " Data
                                               IMPORTING ef_hmacxstring = r_result ).   " HMAC-Value binary as XString
      CATCH cx_abap_message_digest.    "Exception class for Message Digest
    ENDTRY.
  ENDMETHOD.


  METHOD list_bucket_objects.
* exporting et_response_headers  type tihttpnvp
*           e_response_content  type string
* raising zcx_ecs_core_services

    DATA:
      ld_uri       TYPE string,
      ld_host_name TYPE string.

* ECS Header data
    DATA(lt_request_headers) = set_content_type( zif_ecs_core_services=>c_content_type ).

* Create Rest Client
    DATA(lr_client) = create_client( ).

* Set HTTP Method
    DATA(ld_http_method) = zif_ecs_core_services=>c_http_method_get.
    lr_client->request->set_method( ld_http_method ).

* Get server name and port from RFC destination
    ld_host_name = get_server_name_and_port( ). "Throws exception

* Set URI
*    ld_uri = |/{ gs_ecs_config-bucket }?list-type=2|.
    ld_uri = |/{ gs_ecs_config-bucket }|.

    IF gd_debug = abap_true.
      WRITE: / |URI: { ld_uri }|.
    ENDIF.

    cl_http_utility=>set_request_uri( request = lr_client->request
                                      uri     = ld_uri ).

* Set additional header fields
    set_additional_header_fields( it_request_headers = lt_request_headers
                                  ir_client          = lr_client ).

* AWS Signature Version 4
    aws_signature_v4( i_host_name   = ld_host_name    "Throws exception
                      i_http_method = ld_http_method
                      ir_client     = lr_client ).

* Send to server
    send_to_server( lr_client ).  "Throws exception

* Receive Response
    receive_from_server( lr_client ).  "Throws exception

* Returns response header fields
    TRY.
        et_response_headers = get_response_header_fields( lr_client ).  "Throws exception
      CATCH zcx_ecs_core_services.
    ENDTRY.

* Returns response content
    e_response_content = get_response_content( lr_client ).  "Throws exception
  ENDMETHOD.


  METHOD get_response_content.
* importing ir_client  type ref to if_http_client
* returning value(r_response_content)  type string
* raising zcx_ecs_core_services

    DATA(ld_response_xcontent) = ir_client->response->get_data( ).

* Code page conversion external -> system
    DATA(lr_conv_ci) = cl_abap_conv_in_ce=>create( input = ld_response_xcontent ).

    TRY.
        lr_conv_ci->read( IMPORTING data = r_response_content ).   " Returns converted data

      CATCH cx_sy_conversion_codepage    " System-Exception code page conversion
            cx_sy_codepage_converter_init    " System-Exception Init Code Page Converter
            cx_parameter_invalid_type   " Parameter invalid type
            cx_parameter_invalid_range.    " Parameter invalid value range

        RAISE EXCEPTION TYPE zcx_ecs_core_services
          EXPORTING
            textid = zcx_ecs_core_services=>error_convert_codepage.
*   Error code page conversion
    ENDTRY.
  ENDMETHOD.


  METHOD get_response_header_fields.
* importing ir_client  type ref to if_http_client
* returning value(et_response_headers)  type tihttpnvp
* raising zcx_ecs_core_services

    ir_client->response->get_header_fields( CHANGING fields = et_response_headers ).

    DATA(ld_http_status) = ir_client->response->get_header_field( '~status_code' ).

    IF gd_debug = abap_true.
      WRITE: / |Response Status Code: { ld_http_status }|.
    ENDIF.

    IF ld_http_status <> cl_rest_status_code=>gc_success_ok.
      RAISE EXCEPTION TYPE zcx_ecs_core_services
        EXPORTING
          textid = VALUE #( msgid = 'ZECS_CORE_SERVICES' msgno = '008' attr1 = ld_http_status ).
*   HTTP Error status: &
    ENDIF.
  ENDMETHOD.


  METHOD receive_from_server.
* importing ir_client  type ref to if_http_client
* raising zcx_ecs_core_services

    DATA:
      lr_ecs_exception TYPE REF TO zcx_ecs_core_services.

    IF gd_debug = abap_true.
      SKIP 2.
    ENDIF.

    ir_client->receive( EXCEPTIONS http_communication_failure = 1
                                   http_invalid_state         = 2
                                   http_processing_failed     = 3 ).

    IF sy-subrc <> 0.
      IF gd_debug = abap_true.
        WRITE: / |Last Error Receive: { SWITCH #( sy-subrc
                                                  WHEN 1 THEN |http_communication_failure|
                                                  WHEN 2 THEN |http_invalid_state|
                                                  WHEN 3 THEN |http_processing_failed| ) }|.
      ENDIF.

      ir_client->get_last_error( IMPORTING message = DATA(ld_exception_text) ).

      lr_ecs_exception = NEW zcx_ecs_core_services( textid = zcx_ecs_core_services=>error_receive ).
      lr_ecs_exception->set_message_text( ld_exception_text ).

      RAISE EXCEPTION lr_ecs_exception.
*   Error Receive
    ENDIF.
  ENDMETHOD.


  METHOD send_to_server.
* importing ir_client  type ref to if_http_client
* raising zcx_ecs_core_services

    DATA:
      lr_ecs_exception TYPE REF TO zcx_ecs_core_services.

    ir_client->send( EXCEPTIONS http_communication_failure = 1
                                http_invalid_state         = 2
                                http_processing_failed     = 3
                                http_invalid_timeout       = 4 ).

    IF sy-subrc <> 0.
      ir_client->get_last_error( IMPORTING message = DATA(ld_exception_text) ).

      IF gd_debug = abap_true.
        WRITE: / |Last Error Send: { ld_exception_text }|.
      ENDIF.

      lr_ecs_exception = NEW zcx_ecs_core_services( textid = zcx_ecs_core_services=>error_send ).
      lr_ecs_exception->set_message_text( ld_exception_text ).

      RAISE EXCEPTION lr_ecs_exception.
*   Error Send
    ENDIF.
  ENDMETHOD.


  METHOD aws_signature_v4.
* importing i_host_name  type string
*           i_object_name  type string optional
*           i_query_para  type string optional
*           ir_client  type ref to if_http_client
*           i_http_method  type string
* raising zcx_ecs_core_services

    DATA:
      ld_canonicalizeheadernames TYPE string,
      ld_canonicalizedheaders    TYPE string,
      ld_canonicalrequest        TYPE string,
      lt_included_headernames    TYPE tihttpnvp,
      ld_lf                      TYPE string,
      ld_x_amz_date              TYPE string,
      ld_scope                   TYPE string,
      ld_stringtosign            TYPE string,
      ld_ksecret                 TYPE string,
      ld_signature               TYPE string,
      ld_content_bin             TYPE xstring.

* Hash value
    IF i_content_bin IS SUPPLIED.
      ld_content_bin = i_content_bin.
    ENDIF.

    TRY.
        cl_abap_message_digest=>calculate_hash_for_raw( EXPORTING if_algorithm  = 'SHA-256'
                                                                  if_data       = ld_content_bin
                                                        IMPORTING ef_hashstring = DATA(ld_hash) ).
      CATCH cx_abap_message_digest.
        RAISE EXCEPTION TYPE zcx_ecs_core_services
          EXPORTING
            textid = zcx_ecs_core_services=>error_calculate_hash.
*   Fehler bei Hash Wert berechnen
    ENDTRY.

    ld_hash = to_lower( ld_hash ).

    ir_client->request->set_header_field( name = 'x-amz-content-sha256' "#EC NOTEXT
                                          value = ld_hash ).

    IF gd_debug = abap_true.
      WRITE: / |x-amz-content-sha256: { ld_hash }|.
    ENDIF.

* AWS 4 Date - Header field
    ld_x_amz_date = get_tstmp_iso8601basicformat( ).
    ir_client->request->set_header_field( name = 'x-amz-date' "#EC NOTEXT
                                          value = ld_x_amz_date ).

    IF gd_debug = abap_true.
      WRITE: / |x-amz-date: { ld_x_amz_date }|.
    ENDIF.

* AWS 4 Host - Header field
    ir_client->request->set_header_field( name = 'Host'     "#EC NOTEXT
                                          value = |{ i_host_name }| ).

    IF gd_debug = abap_true.
      WRITE: / |Host: { i_host_name }|.
    ENDIF.

* AWS 4 - Build canonical header
    lt_included_headernames = get_included_headernames( ir_client ).
    ld_canonicalizeheadernames = get_canonicalizeheadernames( lt_included_headernames ).
    IF gd_debug = abap_true.
      WRITE: / |Canonicalized Header Names: { ld_canonicalizeheadernames }|.
    ENDIF.

    ld_canonicalizedheaders = get_canonicalizedheaderstring( lt_included_headernames ).
    IF gd_debug = abap_true.
      WRITE: / |Canonicalized Headers: { ld_canonicalizedheaders }|.
    ENDIF.

    ld_lf = cl_abap_char_utilities=>newline.
    ld_canonicalrequest = |{ i_http_method }{ ld_lf }/{ gs_ecs_config-bucket }{ COND #( WHEN i_object_name IS SUPPLIED THEN |/{ i_object_name }| ) }{ ld_lf }|.
    ld_canonicalrequest = |{ ld_canonicalrequest }{ COND #( WHEN i_query_para IS SUPPLIED THEN |{ i_query_para }| ) }{ ld_lf }{ ld_canonicalizedheaders }{ ld_lf }|.
    ld_canonicalrequest = |{ ld_canonicalrequest }{ ld_canonicalizeheadernames }{ ld_lf }{ ld_hash }|.

    IF gd_debug = abap_true.
      DATA(ld_canonicalrequest_write) = ld_canonicalrequest.
      DO.
        IF strlen( ld_canonicalrequest_write ) <= 220.
          WRITE: / |Canonical Reqeust: { ld_canonicalrequest_write }|.
          EXIT.
        ELSE.
          WRITE: / |Canonical Reqeust: { ld_canonicalrequest_write(220) }|.
        ENDIF.

        ld_canonicalrequest_write = shift_left( val = ld_canonicalrequest_write places = 220 ).
      ENDDO.
    ENDIF.

* Builds the scope
    ld_scope = |{ sy-datum }/{ gs_ecs_config-region }/{ zif_ecs_core_services=>c_service }/{ zif_ecs_core_services=>c_terminator }|.

* Request Hash
    TRY.
        cl_abap_message_digest=>calculate_hash_for_char( EXPORTING if_algorithm = 'SHA-256'    " Hash-Algorithm
                                                                   if_data = ld_canonicalrequest    " Data
                                                         IMPORTING ef_hashstring = DATA(ld_hash_canonicalrequest) ).   " Hash value as Hex-Encoded String
      CATCH cx_abap_message_digest.
        RAISE EXCEPTION TYPE zcx_ecs_core_services
          EXPORTING
            textid = zcx_ecs_core_services=>error_calculate_hash.
*   Error hash calculation
    ENDTRY.

* AWS 4 Signature
    ld_hash_canonicalrequest = to_lower( ld_hash_canonicalrequest ).
    ld_stringtosign = |{ zif_ecs_core_services=>c_scheme }-HMAC-{ zif_ecs_core_services=>c_algorithm }{ ld_lf }{ ld_x_amz_date }{ ld_lf }{ ld_scope }{ ld_lf }{ ld_hash_canonicalrequest }|.
    ld_ksecret = |{ zif_ecs_core_services=>c_scheme }{ gs_ecs_config-secret_key }|.

    DATA(ld_xkdate) = hmac_sign_kstring( i_string = |{ sy-datum }|
                                         i_key = ld_ksecret ).
    DATA(ld_xkregion) = hmac_sign_kxstring( i_string = |{ gs_ecs_config-region }|
                                            i_xkey = ld_xkdate ).
    DATA(ld_xkservice) = hmac_sign_kxstring( i_string = zif_ecs_core_services=>c_service
                                             i_xkey = ld_xkregion ).
    DATA(ld_xksigning) = hmac_sign_kxstring( i_string = zif_ecs_core_services=>c_terminator
                                             i_xkey = ld_xkservice ).
    DATA(ld_xsignature) = hmac_sign_kxstring( i_string = ld_stringtosign
                                              i_xkey = ld_xksigning ).
    ld_signature = ld_xsignature.
    ld_signature = to_lower( ld_signature ).

    DATA(ld_authorizationheader) = |{ zif_ecs_core_services=>c_scheme }-HMAC-{ zif_ecs_core_services=>c_algorithm } Credential={ gs_ecs_config-access_key }/{ ld_scope }, SignedHeaders={ ld_canonicalizeheadernames }, Signature={ ld_signature }|.

    ir_client->request->set_header_field( name  = 'Authorization' "#EC NOTEXT
                                          value = ld_authorizationheader ).

    IF gd_debug = abap_true.
      DO.
        IF strlen( ld_authorizationheader ) <= 220.
          WRITE: / |Authorization: { ld_authorizationheader }|.
          EXIT.
        ELSE.
          WRITE: / |Authorization: { ld_authorizationheader(220) }|.
        ENDIF.

        ld_authorizationheader = shift_left( val = ld_authorizationheader places = 220 ).
      ENDDO.
    ENDIF.
  ENDMETHOD.


  METHOD set_additional_header_fields.
* importing it_request_headers  type tihttpnvp
*  ir_client  type ref to if_http_client

    DATA:
      lt_old_header_fields TYPE tihttpnvp.

    ir_client->request->get_header_fields( CHANGING fields = lt_old_header_fields ).
    LOOP AT it_request_headers ASSIGNING FIELD-SYMBOL(<ls_header>).
      IF NOT line_exists( lt_old_header_fields[ name = <ls_header>-name ] ).
        ir_client->request->set_header_field( name = <ls_header>-name
                                              value = <ls_header>-value ).
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  METHOD get_server_name_and_port.
* returning value(r_host_name)  type string
* raising zcx_ecs_core_services

    DATA:
      ld_port        TYPE string,
      lt_value_table TYPE sdest_coded_value_table.

    SELECT SINGLE rfcoptions
      INTO @DATA(ld_rfcoptions)
      FROM rfcdes
      WHERE rfcdest = @gs_ecs_config-destination.

    IF sy-subrc IS NOT INITIAL.
      RAISE EXCEPTION TYPE zcx_ecs_core_services
        EXPORTING
          textid = VALUE #( msgid = 'ZECS_CORE_SERVICES' msgno = '009' attr1 = gs_ecs_config-destination ).
    ELSE.
      SPLIT ld_rfcoptions AT ',' INTO TABLE lt_value_table.
      r_host_name = substring( val = CONV string( lt_value_table[ coded_value(2) = 'H=' ] ) off = 2 ).
      ld_port = substring( val = CONV string( lt_value_table[ coded_value(2) = 'I=' ] ) off = 2 ).
      r_host_name = |{ r_host_name }:{ ld_port }|.
    ENDIF.
  ENDMETHOD.


  METHOD create_client.
* returning value(r_client)  type ref to if_http_client
* raising zcx_ecs_core_services

    cl_http_client=>create_by_destination( EXPORTING destination = gs_ecs_config-destination    " logical destination
                                           IMPORTING client      = r_client     " HTTP Client Abstraction
                                           EXCEPTIONS argument_not_found       = 1
                                                      destination_not_found    = 2
                                                      destination_no_authority = 3
                                                      plugin_not_active        = 4
                                                      internal_error           = 5 ).

    IF sy-subrc IS NOT INITIAL.
      RAISE EXCEPTION TYPE zcx_ecs_core_services
        EXPORTING
          textid = zcx_ecs_core_services=>error_create_by_dest.
*   Error Init Connection (CREATE_BY_DESTINATION)
    ENDIF.
  ENDMETHOD.


  METHOD get_object.
* importing i_object_name  type string
* exporting et_response_headers  type tihttpnvp
*           e_response_content  type xstring
* raising zcx_ecs_core_services

    DATA:
      ld_uri       TYPE string,
      ld_host_name TYPE string.

* ECS Header data
    DATA(lt_request_headers) = set_content_type( zif_ecs_core_services=>c_content_type ).

* Create Rest Client
    DATA(lr_client) = create_client( ).

* Set HTTP Method
    DATA(ld_http_method) = zif_ecs_core_services=>c_http_method_get.
    lr_client->request->set_method( ld_http_method ).

* Get server name and port from RFC destination
    ld_host_name = get_server_name_and_port( ).

* Set URI
    ld_uri = |/{ gs_ecs_config-bucket }/{ i_object_name }|.

    IF gd_debug = abap_true.
      WRITE: / |URI: { ld_uri }|.
    ENDIF.

    cl_http_utility=>set_request_uri( request = lr_client->request
                                      uri     = ld_uri ).

* Set additional header fields
    set_additional_header_fields( it_request_headers = lt_request_headers
                                  ir_client          = lr_client ).

* AWS Signature Version 4
    aws_signature_v4( i_host_name   = ld_host_name  "Throws exception
                      i_object_name = i_object_name
                      i_http_method = ld_http_method
                      ir_client     = lr_client ).

* Send to server
    send_to_server( lr_client ).  "Throws exception

* Receive Response
    receive_from_server( lr_client ).  "Throws exception

* Returns response header fields
    TRY.
        et_response_headers = get_response_header_fields( lr_client ).  "Throws exception
      CATCH zcx_ecs_core_services.
    ENDTRY.

* Returns response content
    e_response_content = lr_client->response->get_data( ).
  ENDMETHOD.

ENDCLASS.
