# Overview
There are already few Cassandra Query Language editors available, but Cyclop is a bit different. It's a web based - once installed you can access it from web browser and still enjoy native application feeling. It's almost fully based on AJAX, so page reloads are rare.

# User Management
![Login](/doc/img/login.png)

Cyclop does not manage users - it passes authorization and authentication to Cassandra. Once Cassandra session has
been opened, it's being stored in HTTP session, and that's it. From now on, each CQL it being passed to Cassandra over its active session, and the query result is successful or not - based on access rights defined in Cassandra for this particular user.
Providing support for things like query history gets a bit tricky, if there is no such thing as user.  We could use credentials used to open Cassandra session, but it's a common use case, that many users share them - like "read only user for IT on third third floor".
As you might noticed, the UUID is a solution to all our problems, and this time it worked too! Cyclop generates random cookie based on UUID and stores it in browser. This is the replacement solution for missing user management. We do not recognize user itself, but the browser. Of curse valid Cassandra session is always required, so it's not possible that unauthorized user could access restricted data (like query history) only because he has access to the browser, or "knows" the cookie value, he would have to login in the first place.  

# Query Editor
### Query Completion
![CQL Completion](/doc/img/comp_colors.png)
* CQL keyword completion is supported for almost whole CQL3 syntax
* Completion Hint shows all possible keywords, that are valid for actual query position. Tables, keyspaces and columns are grouped together, and sorted. Groups are also highlighted with different font color.
* if the keyspace has been set in previous query ("use cqldemo" in screen shot below), the completion for the following queries
will be narrowed to tables from this keysapce, assuming that keyspace is not explicitly provided in query
![CQL Completion](/doc/img/comp_tables_from_global_keyspace.png)
* completion contains only tables that belong to keyspace that has been provided in current query
![CQL Completion](/doc/img/completion_cqldemo_tables.png)
* completion contains only columns of a table that has been provided in current query
![CQL Completion](/doc/img/comp_table_columns.png)
* query syntax help has been copied from Cassandra documentation. It is decorated with color highlighting
matching Completion Hint colors
![CQL Syntax Help](/doc/img/cql_syntax_help.png)

### Keyboard Navigation
* Enter - confirms currently highlighted completion
* Tab - next completion value
* Ctrl+Enter - executes query
* ESC - cancel completion

### Configuration
<code>cyclop/src/main/resources/cyclop.properties</code>

``` properties
cqlEditor.rowsPerPage: 5
cqlEditor.maxColumnDisplayChars: 256
cqlEditor.maxColumnEmbeddedDisplayChars: 64
cqlEditor.maxColumnTooltipDisplayChars: 1024
```

# Query Results Table
![Results Table](/doc/img/results_table.png)
* results table is column-oriented, it's reversed when compared to traditional SQL editors - rows are displayed horizontally,
and columns vertically. When scrolling page from left to right you will switch between rows. Scrolling from top to bottom
shows follow up columns
* columns are displayed in order returned by the query, but additionally they are grouped into two sections
divided by blue separator line. The top of the table contains "static columns" - their values are not empty in multiple
rows returned by the query. The second section contains columns, which value is not empty only for single row. Cassandra
supports dynamic columns, and the idea is to have "static" columns at the top of the table, and "dynamic" ones on the bottom, because those are mostly empty
* table header for each row displays partition key value, assuming that query returns it
* long text is trimmed in order to fit into table cell. Such cell has in left top corner a blue icon, clicking on it opens pop-up containing the whole text
![Large Content](/doc/img/large_content.png)

# Query History
![Query History Full](/doc/img/history_full.png)
* history contains last 500 queries that has been successfully executed from this particular browser (we recognize users based on persistent cookie)
* each entry in history contains the query itself, the runtime and response size
* next to the query there is a blue icon, clicking on it will trigger redirect to editor and paste query into it, so you can execute it again
### History Filtering
* filter supports live update - you get results while typing. Just remember that words shorter that three characters will be ignored
* multiple keywords are joined by OR, this means that filter result will contain queries which contain at leas one keyword
* you can specify multiple keywords in the filter. Is such case the top of the filtered history will contain queries most hits. This builds groups, like queries with four hits, than three, and on the end with single hit. The queries within those groups are sorted alphabetically
![Query History Filter](/doc/img/history_filter.png)
* pressing Enter resets filter, you can also click on "clean" icon

### Data on the Server
The history itself is stored on server in configured folder (fileStore.folder), in file: [fileStore.folder]\QueryHistory-[User-UUID].json.  The file itself contains serialized history in json form.
The solution is also secure, so you can use Cyclop from any computer without restrictions. Random cookie is the only information stored in browser - but this does not matter, because history can be viewed only by authenticated users.

### Configuration
<code>cyclop/src/main/resources/cyclop.properties</code>

``` properties
history.entriesLimit: 500
history.enabled: true
history.queriesPerPage: 50
fileStore.maxFileSize: 10485760
fileStore.lockWaitTimeoutMilis: 5000
fileStore.folder: /tmp
```

# Bookmarks
![Query Bookmark](/doc/img/query_bookmark.png)
CQL query can be bookmarked. This is convenient for frequently used queries, or if you like to share it with somebody else. When you click on bookmark button, the URL in browser will be changed, so that it contains the query from editor.

# CSV Export
![CSV Export](/doc/img/csv_export.png)

Query result can be exported to CSV file.

### Configuration
<code>cyclop/src/main/resources/cyclop.properties</code>

``` properties
cqlExport.querySeparator:CR====CR
cqlExport.rowSeparator:CR
cqlExport.crCharCode:10
cqlExport.listSeparator:,
cqlExport.mapSeparator:=
cqlExport.columnSeparator:;
cqlExport.removeCrChars: true
cqlExport.trim: true
cqlExport.valueBracketStart:"
cqlExport.valueBracketEnd:"
cqlExport.fileName: cql_export_DATE.csv
cqlExport.fileName.date: yyyy-MM-dd_HH:mm:ss.SSS
```

# Requirements
* java 7
* cassandra v1.2 or above (tested with 1.2 and 2.0)
* supports only schema created with CQL 3
* CLI schamas are NOT supported
* Tomcat v7 or another v3.x web container

# Technologies
* web app - v3.x
* maven - v3.x
* spring - v3.x
* wicket - v6.x
* wicket-auth-roles - v6.x
* bootstrap - v3.x (theme: cyborg from bootswatch)
* jQuery UI - v1.10.x
* cassandra-driver-core - v1.x
* slf4j/logback - v1.7.x
* hibernate validator - v4.x
* guava - v15.x (Cassandra 2.0 does not work with v16)


# Installation
1. Download last release: <code>https://github.com/maciejmiklas/cyclop/releases/latest</code>
2. Edit property file: <code>cyclop/src/main/resources/cyclop.properties</code> and set connection settings for Cassandra
``` properties
cassandra.hosts: localhost
cassandra.port: 9042
cassandra.useSsl: false
cassandra.timeoutMilis: 3600000
```
You can also overwrite each property from <code>cyclop.properties</code> by setting it as jvm parameter, for example
to connect to different Cassandra host set:<code>-Dcassandra.hosts=server1,server2</code>. This gives you simple possibility
to change properties after the war file has been assembled.

4. Optionally change logger settings by editing <code>logback.xml</code>. By default it logs in into into <code>/var/lib/tomcat7/logs/cyclop-${time}.log</code>
5. Build war file: <code>mvn package</code>
5. Drop war file into tomcat

The created war can connect only to one Cassandra cluster, in order to serve multiple clusters from one Tomcat
you have to deploy few cyclop war archives, each one with different  <code>cassandra.hosts</code> value
