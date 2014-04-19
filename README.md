# Overview
Cassandra is one of the most popular open source NoSQL databases, but it's only few years old, and therefore tools support is still limited, especially when it comes to free open source software. 

If you are working with Cassandra, sooner or later you will have to analyse its content on remote cluster. Most of the available tools are desktop applications  that connect to Cassandra over its protocol. Getting such access might be a hard task, because usually databases are installed in restricted networks in order to minimize data breach risk. Security policies are changing frequently to cover new findings, and the fact that you have access to your database today, does not actually mean that it will last long.
Gaining access over SSH and command line interface should be easier, but I do not have to convince anyone that using terminal to query database content is painful, especially when it comes to NoSQL database which contains tons of data, and it's wide rows can contain millions of columns!

But there is one solution, that is almost always available: web based applications! Every company knows how to secure them, how to run penetration tests, locate security leaks, and so on.... actually it does not matter what happens behind scenes, you - the end user has always access to such application.

Here is a good news: Cyclop is 100% web based, and It's based on latest Wicket release! Once you managed to install it, you can query your database from a web browser and still enjoy native application feeling. It's almost fully based on AJAX, so page reloads are rare.

There is also another cool thing: it your security experts will run penetration tests against Cyclop they will came up with findings like Database Script Injection. This will be the first time in you live when you can honestly say: "It's not a bug, it's a future!". Anyway .... I would suggest  to restrict access to Cyclop to some trusted networks. It's definitely no usual web application, but once you have managed to deploy it, you can enjoy simple access to you data over CQL.

# User Management
![Login](/doc/img/login.png)

Cyclop does not manage users - it passes authorization and authentication to Cassandra. Once Cassandra session has
been opened, it's being stored in HTTP session, and that's it. From now on, each query it being passed to Cassandra over its active session, and the result is successful or not - based on access rights defined in Cassandra for this particular user.

Providing support persistent data like query history gets a bit tricky, if there is no such thing as user.  We could reference credentials used to open Cassandra session, but it's a common use case, that many users share them - like "read only user for IT on third third floor".

As you might noticed, the UUID is a solution to all our problems, and this time it worked too! Cyclop generates random cookie based on UUID and stores it in browser. This is the replacement solution for missing user management. We do not recognize user itself, but the browser. Of curse valid Cassandra session is always required, so it's not possible that unauthorized user could access restricted data (like query history) only because he has access to the browser, or "knows" the cookie value, he would have to login in the first place.  

#### User Preferences

# Query Editor
#### Query Completion
* completion is supported for almost whole CQL 3 syntax
* Completion Hint shows all possible keywords, that are valid for actual query position. Tables, keyspaces and columns are grouped together, and sorted. Groups are also highlighted with different font color
![CQL Completion](/doc/img/cmp_colors.png)
* if the keyspace has been set in previous query ("use cqldemo" in screen shot below), the completion for the following queries
will be narrowed to tables from this keysapce, assuming that keyspace is not explicitly provided in query
![CQL Completion](/doc/img/cmp_tables_from_global_keyspace.png)
* completion contains only tables that belong to keyspace that has been provided in current query
![CQL Completion](/doc/img/cmp_cqldemo_tables.png)
* completion contains only columns of a table that has been provided in current query
![CQL Completion](/doc/img/cmp_table_columns.png)
* query syntax help has been copied from Cassandra documentation. It is decorated with color highlighting
matching Completion Hint colors
![CQL Syntax Help](/doc/img/cql_syntax_help.png)

#### Keyboard Navigation
* Enter - confirms currently highlighted completion
* Tab - next completion value
* Ctrl+Enter - executes query
* ESC - cancel completion

# Query Results Table
![Results Table](/doc/img/results_table.png)
* results table is column-oriented, it's reversed when compared to traditional SQL editors - rows are displayed horizontally,
and columns vertically. When scrolling page from left to right you will switch between rows. Scrolling from top to bottom
shows follow up columns
* columns are displayed in order returned by the query, but additionally they are grouped into two sections
divided by blue separator line. The top of the table contains "static columns" - their values are not empty in multiple
rows returned by executed query. The second section contains columns, which value is not empty only for single row. Cassandra
supports dynamic columns, and the idea is to have "static" columns at the top of the table, and "dynamic" ones on the bottom, because those are mostly empty
* table header for each row displays partition key value, assuming that query returns it
* long text is trimmed in order to fit into table cell. Such cell has a blue icon in the left top corner, clicking on it opens pop-up containing the whole text
![Large Content](/doc/img/large_content.png)

# Query History
![Query History Full](/doc/img/history_full.png)

* history contains last 500 queries that has been successfully executed from this particular browser (we recognize users based on persistent cookie)
* each entry in history contains the query itself, the runtime and response size
* next to the query there is a blue icon, clicking on it will trigger redirect to editor and paste query into it, so you can execute it again

#### History Filtering
* filter supports live update - you get results while typing. Just remember that words shorter than three characters will be ignored
* multiple keywords are joined by OR, this means that filter result will contain queries which contain at leas one keyword
* you can specify multiple keywords in the filter. Is such case the top of the filtered history will contain queries with most hits. This builds groups, like queries with four hits, than three, and on the end with single hit. The queries within those groups are sorted by execution time
![Query History Filter](/doc/img/history_filter.png)
* pressing Enter resets filter, you can also click on "clean" icon

#### Data on the Server
The history itself is stored on server in configured folder (fileStore.folder), in file: [fileStore.folder]\QueryHistory-[User-UUID].json.  The file itself contains serialized history in json form.
The solution is also secure, so you can use Cyclop from any computer without restrictions. Random cookie is the only information stored in browser - but this does not matter, because history can be viewed only by authenticated users.

# Bookmarks
![Query Bookmark](/doc/img/query_bookmark.png)
CQL query can be bookmarked. This is convenient for frequently used queries, or if you like to share it with somebody else. When you click on bookmark button, the URL in browser will be changed, so that it contains the query from editor.

# CSV Export
![CSV Export](/doc/img/csv_export.png)

Query result can be exported to CSV file.

# Import
It's meant to import files containing CQL queries separated by ;\n. Single query can spread over multiple lines. Results of the import are displayed in table, which contains each single query, runtime and eventually an error - in this case row has read color. You can also specify few options, so that script execution will break (or not) after first error, or executed queries can be included in query history.

Import has also few limitations:
* import script and results table has to fit into tomcat memory
* each query will be executed as separate Cassandra call, so that we can precisely point out errors, and measure execution time, on the other hand side it causes latencies
* import script does not support comments

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

# Live Demo
There is a demo deployment of Cyclop, so that you can get a first impression. I'm hosting it at home, so it can be down sometimes, because I have no static IP, and when it changes propagation takes some time.

Different links below contain different queries. Clicking on link will open Cyclop and paste into its editor query from link. Try to edit those queries using Cyclop's editor to see how the code completion is working. Provided user has read only access, so only part of the functionality is available.
* User: democasusr, Password: Cassandra123 written backwards (32...aC)
* http://maciejmiklas.no-ip.biz/cyclop
* http://maciejmiklas.no-ip.biz/cyclop/main/ced?cql=select%20*%20from%20cqldemo.mybooks
* http://maciejmiklas.no-ip.biz/cyclop/main/ced?cql=select%20id%2Cauthors%2Cgenre%20from%20cqldemo.mybooks%20where%20pages%20%3D%20121
* http://maciejmiklas.no-ip.biz/cyclop/main/ced?cql=select%20id%2Cauthors%20from%20cqldemo.mybooks%20where%20id%3D6ff12f41-cfb1-45ff-9e89-fb20f95ffc5d
* http://maciejmiklas.no-ip.biz/cyclop/main/ced?cql=select%20*%20from%20system.schema_columnfamilies
