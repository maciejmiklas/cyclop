# Overview
Cassandra has already a few CQL3 editors, but Cyclop is a bit different.
Itís a lightweight web based  editor - once installed you can access it from web browser and still enjoy native application feeling.

# Authorization
![Login](/doc/img/login.png)

User name and password are used to create Cassandra session,  which is bind to active HTTP Session. Cyclop itself does
not keep or persist credentials. Only remember-me function does it as encrypted cookie in browser.

# User Management
Cyclop does not manage users - it passes authorization and authentication to Cassandra. Once Cassandra session has been opened, itís being stored in HTTP session, and thatís it. From now on, each CQL it being passed to Cassandra over this active session, and the query result is successful or not - based on access rights defined in Cassandra for this particular user. 
Providing support for things like query history gets a bit tricky, if there is no such thing as user.  We could use credentials used to open Cassandra session, but it's a common use case, that many users share them ñ like "read only user for IT on third third floor".
The UUID is a solution to all our problems, and this time it worked too! Cyclop generates random cookie based on UUID and stores it in browser. This is the replacement solution for missing user management. We do not recognize user itself, but the browser. Of curse valid Cassandra session is always required, so it's not possible that unauthorized user could access restricted data (like query history) only because he has access to the browser, or "knows" the cookie value, he would have to login in the first place. 

# Query Editor
* supports context completion for CQL3 syntax. For example:
** if the keyspace has been set ("use myKespace"), the completion contains only tables from specified keyspace, or
** completion contains only tables that belong to keyspace that has been provided in the query (select * from myKespace.xyz...), or 
** completion contains only columns of a table that has been provided in the query (insert into myTable values .... )
* keyboard navigation:
   * Enter - confirms currently highlighted completion
   * Tab - next completion value
   * Ctrl+Enter - executes query
   * ESC - cancel completion
* completion hint - the green pop up on the right top corner shows all possible completion values. Font has different colors
for table, keyspace, column, type and keyword. Also the keywords are grouped together, and then sorted. Groups are build for: table, column and keyspace names
* query syntax help - contains CQL syntax help copied from original Cassandra documentation. It is decorated with color
highlighting matching "completion hint" colors

![CQL Completion Colors](/doc/img/completion_colors.png)

![CQL Completion](/doc/img/completion_space_tables.png)

![CQL Syntax Help](/doc/img/cql_syntax_help.png)

# Query Result Table
* results table is column-oriented, itís reversed when compared to traditional SQL editors - rows are displayed horizontally,
and columns vertically. When scrolling page from left to right you will switch between rows. Scrolling from top to bottom
shows follow up columns
* columns are displayed in order returned by the query, but additionally they are grouped into two sections
divided by blue separator line. The top of the table contains "static columns" - their values are not empty in multiple
rows returned by the query. The second section contains columns, which value is not empty only for single row. Cassandra
supports dynamic columns, and the idea is to have "static" columns at the top of the table, and "dynamic" ones on the bottom, because those are mostly empty
* table header for each row displays partition key value, assuming that query returns it
* long text is trimmed in order to fit into table cell. Such cell has in left top corner a blue icon, clicking on it opens pop-up containing the whole text

![Cyclop Pain Page](/doc/img/results_table.png)

![Popup Display](/doc/img/large_content.png)

# Query History
The history itself is stored on server in configured folder (fileStore.folder), in file: [fileStore.folder]\QueryHistory-[User-UUID].json.  The file itself contains serialized history object as json.
The solution is also secure, so you can use Cyclop from any computer without restrictions. Random cookie is the only information stored in browser ñ but this does not matter, because history can be viewed only by authenticated users. 

# Bookmarks
* CQL query can be bookmarked. This is convenient for frequently used queries, or if you like to share it with somebody else

# CSV export
* Query results can be exported to CSV file

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

4. Optionally change logger settings by editing <code>logback.xml</code> (it's using console appender)
5. Build war file: <code>mvn package</code> 
5. Drop war file into tomcat

The created war can connect only to one Cassandra cluster, in order to serve multiple clusters from one Tomcat
you have to deploy few cyclop war archives, each one with different  <code>cassandra.hosts</code> value
