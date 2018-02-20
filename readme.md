## Voraussetzungen:
Eine RFC-Destination (Verbindungstyp „G“)
Konfiguration in Tabelle ZECS_CONFIG (SM30)

## Entwicklungsobjekte:
##### Paket ZECS:
Enthält alle Entwicklungsobjekte.

##### Tabelle ZECS_CONFIG:
Enthält die Konfiguration

|Feld|Bezeichnung|
|-|-|
|mandt|Mandant|
|bucket|ECS: Bucket Name|
|destination|logische Destination (Wird bei Funktionsaufruf angegeben)|
|region|ECS: Region|
|server_encrypt|ECS: Server Encryption|
|client_encrypt|ECS: Client Encryption|
|zip|ECS: ZIP|
|access_key|ECS: Access Key|
|secret_key|ECS: Secret Key|
|default_bucket|Default Bucket|

|Datenelemente:|
|-|
|ZECS_ACCESS_KEY|
|ZECS_BUCKET|
|ZECS_CLIENT_ENCRYPT|
|ZECS_DEFAULT_BUCKET|
|ZECS_REGION|
|ZECS_SECRET_KEY|
|ZECS_SERVER_ENCRYPT|
|ZECS_ZIP|

|Domänen:|
|-|
|ZECS_ACCESS_KEY|
|ZECS_BUCKET|
|ZECS_REGION|
|ZECS_SECRET_KEY|

|Tabellentyp:|
|-|
|ZECS_T_SSFBIN|

##### Interface ZIF_ECS_CORE_SERVICES:
Hier werden Konstanten und die Core Methoden definiert.

##### Klasse ZCL_ECS_CORE_SERVICES:
Stellt mit der Methode FACTORY ein Objekt zur Verfügung mit dem die Methoden PUT_OBJECT, GET_OBJECT und LIST_BUCKET_OBJECTS zur Verfügung gestellt werden.

##### Ausnahmeklasse ZCX_ECS_CORE_SERVICES:
Hiermit werden alle Fehler aus den Core Services behandelt.

##### Funktionsgruppe ZECS:
Dient nur dem SM30 Tabellen Pflegedialog.

##### Programm ZECS_EXPLORER:
Ein Beispiel Programm für die Verwendung der Core Services. Damit können Objekte im Object Storage abgelegt werden, Objekte aus dem Object Storage gelesen werden und eine Liste der am Object Storage gespeicherten Objekte ausgegeben werden.

##### Transaktion ZECS_EXPLORER:
Ruft das Programm ZECS_EXPLORER auf.

### Zusätzliche Funktionen in der Konfiguration:
Option ZIP: Damit können Files komprimiert an den Object Storage übergeben werden.
Option SERVER_ENCRYPT: Hiermit wird dem Object Storage mitgeteilt, dass er die Files verschlüsseln soll.
Option CLIENT_ENCRYPT: Damit kann die Verschlüsselung des zu speichernden Objekts bereits am Client (SAP) durchgeführt werden. !!! Diese Option ist noch nicht fertig. !!!

### Installation

Installation via abapGit

### Todos

 - Write MORE Tests
 - Add function for client encryption

License
----

MIT
