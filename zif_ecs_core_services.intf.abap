INTERFACE zif_ecs_core_services
  PUBLIC .

  CONSTANTS:
    c_scheme          TYPE string VALUE 'AWS4',
    c_algorithm       TYPE string VALUE 'SHA256',
    c_terminator      TYPE string VALUE 'aws4_request',
    c_service         TYPE string VALUE 's3',
    c_content_type    TYPE string VALUE 'application/octet-stream',
    c_http_method_get TYPE string VALUE 'GET',
    c_http_method_put TYPE string VALUE 'PUT'.

  "! Upload file or create folder
  "! @parameter i_object_name | Object or folder name (folder + '/')
  "! @parameter i_content_bin | File as xstring
  "! @parameter et_response_headers | Response header table
  "! @parameter e_response_content | Response body
  "! @raising zcx_ecs_core_services | Exception
  METHODS put_object
    IMPORTING i_object_name       TYPE string
              i_content_bin       TYPE xstring
    EXPORTING et_response_headers TYPE tihttpnvp
              e_response_content  TYPE string
    RAISING   zcx_ecs_core_services.

  "! List content of bucket (Files and folders)
  "! @parameter et_response_headers | Response header table
  "! @parameter e_response_content | Response body
  "! @raising zcx_ecs_core_services | Exception
  METHODS list_bucket_objects
    EXPORTING
      et_response_headers TYPE tihttpnvp
      e_response_content  TYPE string
    RAISING
      zcx_ecs_core_services.

  "! Download file
  "! @parameter i_object_name | Object name
  "! @parameter et_response_headers | Response header table
  "! @parameter e_response_content | Response body as xstring
  "! @raising zcx_ecs_core_services | Exception
  METHODS get_object
    IMPORTING
      i_object_name       TYPE string
    EXPORTING
      et_response_headers TYPE tihttpnvp
      e_response_content  TYPE xstring
    RAISING
      zcx_ecs_core_services.

ENDINTERFACE.
