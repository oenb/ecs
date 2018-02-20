************************************************************************
*
* Österreichische Nationalbank
*
************************************************************************
* Programm...: z_ecs_test
* Autor......:
* Beschreibung:
*
*
************************************************************************
* Ä N D E R U N G E N
*-----------------------------------------------------------------------
* JJJJMMTT-nnnn – Grund (Change)
* - Änderungstext
*
************************************************************************

REPORT zecs_explorer LINE-SIZE 255.

* Bucket Selection
SELECTION-SCREEN BEGIN OF BLOCK b01 WITH FRAME TITLE text-b01.
PARAMETERS:
  p_bucket TYPE zecs_bucket.
SELECTION-SCREEN END OF BLOCK b01.

SELECTION-SCREEN BEGIN OF BLOCK b02 WITH FRAME TITLE text-b02.
* File Upload
PARAMETERS:
  p_upload RADIOBUTTON GROUP r01,
  p_fileup TYPE localfile.

* File download
PARAMETERS:
  p_dnload RADIOBUTTON GROUP r01,
  p_filedn TYPE localfile.

* Create Folder
PARAMETERS:
  p_crtfol RADIOBUTTON GROUP r01,
  p_folder TYPE localfile.

* List Bucket Content
PARAMETERS:
  p_list RADIOBUTTON GROUP r01.
SELECTION-SCREEN END OF BLOCK b02.

PARAMETERS:
  p_debug AS CHECKBOX DEFAULT abap_false.

CLASS lcl_ecs_test DEFINITION CREATE PUBLIC.

  PUBLIC SECTION.
    "! Upload file
    METHODS get_file.
    "! Main processing
    METHODS main.
    "! Constructor
    METHODS constructor.
    "! Get directory to save file
    METHODS get_directory
      RETURNING VALUE(e_directory) TYPE string.
    METHODS get_default_bucket
      RETURNING
        VALUE(r_result) TYPE zecs_bucket.

  PRIVATE SECTION.
    DATA:
      gd_content_bin TYPE xstring,
      gd_dft_bucket  TYPE zecs_bucket.
ENDCLASS.

CLASS lcl_ecs_test IMPLEMENTATION.

  METHOD constructor.
    SELECT bucket
      INTO @gd_dft_bucket
      FROM zecs_config UP TO 1 ROWS
      WHERE default_bucket = @abap_true.
    ENDSELECT.
  ENDMETHOD.

  METHOD get_file.

    DATA:
      lt_file_table  TYPE filetable,
      ld_rc          TYPE i,
      lt_data_tab    TYPE TABLE OF x255,
      ld_user_action TYPE i.

    cl_gui_frontend_services=>file_open_dialog( CHANGING file_table  = lt_file_table    " Tabelle, die selektierte Dateien enthält
                                                         rc          = ld_rc    " Rückgabewert: Anzahl Dateien oder -1 falls Fehler auftritt
                                                         user_action = ld_user_action
                                                EXCEPTIONS file_open_dialog_failed = 1
                                                           cntl_error              = 2
                                                           error_no_gui            = 3
                                                           not_supported_by_gui    = 4 ).

    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                 WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    IF ld_user_action = cl_gui_frontend_services=>action_ok.
      cl_gui_frontend_services=>gui_upload( EXPORTING filename   = CONV #( lt_file_table[ 1 ]-filename )    " Name der Datei
                                                      filetype   = 'BIN'    " Dateityp (Ascii, Binär)
                                            IMPORTING filelength = DATA(ld_filelength)    " Dateilänge
                                            CHANGING  data_tab   = lt_data_tab    " Übergabetabelle für Datei-Inhalt
                                            EXCEPTIONS file_open_error         = 1
                                                       file_read_error         = 2
                                                       no_batch                = 3
                                                       gui_refuse_filetransfer = 4
                                                       invalid_type            = 5
                                                       no_authority            = 6
                                                       unknown_error           = 7
                                                       bad_data_format         = 8
                                                       header_not_allowed      = 9
                                                       separator_not_allowed   = 10
                                                       header_too_long         = 11
                                                       unknown_dp_error        = 12
                                                       access_denied           = 13
                                                       dp_out_of_memory        = 14
                                                       disk_full               = 15
                                                       dp_timeout              = 16
                                                       not_supported_by_gui    = 17
                                                       error_no_gui            = 18 ).

      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                   WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.

* Convert data table to xstring
      CALL FUNCTION 'SCMS_BINARY_TO_XSTRING'
        EXPORTING
          input_length = ld_filelength
        IMPORTING
          buffer       = gd_content_bin
        TABLES
          binary_tab   = lt_data_tab
        EXCEPTIONS
          failed       = 1.

      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                   WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD main.
    DATA:
      ld_file                 TYPE localfile,
      lt_response_headers     TYPE tihttpnvp,
      ld_response_content     TYPE string,
      ld_localdir             TYPE string,
      ld_response_content_bin TYPE xstring,
      lt_data                 TYPE TABLE OF x255,
      ld_content_bin_length   TYPE i.

    CASE abap_true.
      WHEN p_upload.
        get_file( ).
      WHEN p_dnload.
        ld_localdir = get_directory( ).
    ENDCASE.

* Factory of Core Services
    TRY.
        DATA(lr_ecs_service) = zcl_ecs_core_services=>factory( i_bucket = p_bucket
                                                               i_debug  = p_debug ).
      CATCH zcx_ecs_core_services.    "Ausnahmeklasse Core Services
    ENDTRY.

    IF p_upload = abap_true.
      ld_file = p_fileup.
    ENDIF.

    IF p_dnload = abap_true.
      ld_file = p_filedn.
    ENDIF.

* Folders need a '/' at the end
    IF p_crtfol = abap_true.
      ld_file = p_folder.
      IF substring( val = ld_file off = strlen( ld_file ) - 1 len = 1 ) <> '/'.
        ld_file = |{ ld_file }/|.
      ENDIF.
    ENDIF.

* Creating folders and uploading files is the same
    IF p_crtfol = abap_true OR p_upload = abap_true.
      TRY.
          lr_ecs_service->put_object( EXPORTING i_object_name       = CONV #( ld_file )
                                                i_content_bin       = gd_content_bin
                                      IMPORTING et_response_headers = lt_response_headers
                                                e_response_content  = ld_response_content ).
        CATCH zcx_ecs_core_services.    "Ausnahmeklasse Core Services
      ENDTRY.
    ENDIF.

* Downloading file and save in selected local directory
    IF p_dnload = abap_true.
      TRY.
          lr_ecs_service->get_object( EXPORTING i_object_name       = CONV #( ld_file )
                                      IMPORTING et_response_headers = lt_response_headers
                                                e_response_content  = ld_response_content_bin ).

          CALL FUNCTION 'SCMS_XSTRING_TO_BINARY'
            EXPORTING
              buffer        = ld_response_content_bin
            IMPORTING
              output_length = ld_content_bin_length
            TABLES
              binary_tab    = lt_data.

          cl_gui_frontend_services=>gui_download( EXPORTING bin_filesize              = ld_content_bin_length    " Dateilänge bei Binärdateien
                                                            filename                  = CONV #( ld_file )    " Name der Datei
                                                            filetype                  = |BIN|    " Dateityp (Ascii, Binär, ...)
                                                  CHANGING  data_tab                  = lt_data    " Übergabetabelle
                                                  EXCEPTIONS file_write_error          = 1
                                                             no_batch                  = 2
                                                             gui_refuse_filetransfer   = 3
                                                             invalid_type              = 4
                                                             no_authority              = 5
                                                             unknown_error             = 6
                                                             header_not_allowed        = 7
                                                             separator_not_allowed     = 8
                                                             filesize_not_allowed      = 9
                                                             header_too_long           = 10
                                                             dp_error_create           = 11
                                                             dp_error_send             = 12
                                                             dp_error_write            = 13
                                                             unknown_dp_error          = 14
                                                             access_denied             = 15
                                                             dp_out_of_memory          = 16
                                                             disk_full                 = 17
                                                             dp_timeout                = 18
                                                             file_not_found            = 19
                                                             dataprovider_exception    = 20
                                                             control_flush_error       = 21
                                                             not_supported_by_gui      = 22
                                                             error_no_gui              = 23 ).

          IF sy-subrc <> 0.
            MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                       WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
          ENDIF.
        CATCH zcx_ecs_core_services.    "Ausnahmeklasse Core Services
      ENDTRY.
    ENDIF.

* Get a list of objects in bucket
    IF p_list = abap_true.
      TRY.
          lr_ecs_service->list_bucket_objects( IMPORTING et_response_headers = lt_response_headers
                                                         e_response_content  = ld_response_content ).

        CATCH zcx_ecs_core_services.    "Ausnahmeklasse Core Services
      ENDTRY.
    ENDIF.

    IF p_debug = abap_true.
      LOOP AT lt_response_headers ASSIGNING FIELD-SYMBOL(<ls_response_header>).
        WRITE: / <ls_response_header>-name, ':', <ls_response_header>-value.
      ENDLOOP.
    ENDIF.

* Display XML Response
    IF ld_response_content IS NOT INITIAL.
      DATA(lr_xml_doc) = NEW cl_xml_document( ).
      lr_xml_doc->parse_string( ld_response_content ).
      lr_xml_doc->display( ).
    ENDIF.
  ENDMETHOD.


  METHOD get_directory.
* exporting e_directory  type string

    cl_gui_frontend_services=>directory_browse( EXPORTING window_title   = |Save file to|    " Titel des Browser-Fensters
                                                CHANGING selected_folder = e_directory    " Vom Benutzer selektiertes Verzeichnis
                                                EXCEPTIONS cntl_error           = 1
                                                           error_no_gui         = 2
                                                           not_supported_by_gui = 3 ).
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                 WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
  ENDMETHOD.


  METHOD get_default_bucket.
* returning value(r_result)  type zecs_bucket

    r_result = gd_dft_bucket.
  ENDMETHOD.

ENDCLASS.

INITIALIZATION.
  DATA(gr_ecs_test) = NEW lcl_ecs_test( ).
  p_bucket = gr_ecs_test->get_default_bucket( ).

START-OF-SELECTION.
  IF p_bucket IS INITIAL.
    MESSAGE ID 'ZECS_CORE_SERVICES' TYPE 'I' NUMBER 010 DISPLAY LIKE 'E'.
  ELSE.
    gr_ecs_test->main( ).
  ENDIF.
