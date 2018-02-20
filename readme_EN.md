## Preconditions:
A RFC destination (connectiontype „G“)
Configuration in table ZECS_CONFIG (SM30)

## Development objects:
##### Package ZECS:
Contains all development objects.

##### Table ZECS_CONFIG:
Contains configuration.

|Field|Short Description|
|-|-|
|mandt|Client|
|bucket|ECS: Bucket Name|
|destination|Connection to external server|
|region|ECS: Region|
|server_encrypt|ECS: Server Encryption|
|client_encrypt|ECS: Client Encryption|
|zip|ECS: ZIP|
|access_key|ECS: Access Key|
|secret_key|ECS: Secret Key|
|default_bucket|Default Bucket|

|Data Elements:|
|-|
|ZECS_ACCESS_KEY|
|ZECS_BUCKET|
|ZECS_CLIENT_ENCRYPT|
|ZECS_DEFAULT_BUCKET|
|ZECS_REGION|
|ZECS_SECRET_KEY|
|ZECS_SERVER_ENCRYPT|
|ZECS_ZIP|

|Domains:|
|-|
|ZECS_ACCESS_KEY|
|ZECS_BUCKET|
|ZECS_REGION|
|ZECS_SECRET_KEY|

|Table Type:|
|-|
|ZECS_T_SSFBIN|

##### Interface ZIF_ECS_CORE_SERVICES:
Definition of constants and core methods.

##### Class ZCL_ECS_CORE_SERVICES:
Provides with method FACTORY an object to use the methods PUT_OBJECT, GET_OBJECT and LIST_BUCKET_OBJECTS.

##### Exception Class ZCX_ECS_CORE_SERVICES:
Error handling for core services.

##### Funkcion group ZECS:
Only for SM30 table maintenance generator.

##### Program ZECS_EXPLORER:
A sample report using the core services. You can store, load and list objects on your object storage system.

##### Transaction ZECS_EXPLORER:
Calls program ZECS_EXPLORER.

### Additional Functions in configuration table:
Option ZIP: Compress files before sending to server.
Option SERVER_ENCRYPT: Tell the object storage system zu encrypt the file.
Option CLIENT_ENCRYPT: Encryption on the client (SAP). !!! This option isn't ready yet. !!!

### Installation

Installation via abapGit

### Todos

 - Write MORE Tests
 - Add function for client encryption

License
----

MIT
