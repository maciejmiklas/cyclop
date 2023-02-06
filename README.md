# Overview
Cassandra is one of the most popular open-source NoSQL databases, but it's only a few years old, and therefore tools support still needs to be improved, especially when it comes to free, open-source software. 

If you are working with Cassandra, you will have to analyze its content on a remote cluster sooner or later. Most available tools are desktop applications that connect to Cassandra over its protocol. Getting such access might be challenging because databases are usually installed in restricted networks to minimize data breach risk. Security policies are frequently changing to cover new findings, and the fact that you have access to your database today does not mean that it will only last for a while.

Gaining access over SSH and the command line interface should be more accessible. Still, I do not have to convince anyone that using a terminal to query database content is painful, especially when it holds wide rows!

But there is one solution that is almost always available: web-based applications! Every company knows how to secure them, run penetration tests, locate security leaks, and so on. It does not matter what happens behind the scenes; the end user always has access to such applications - at least in theory ;)

Here comes the good news: Cyclop is 100% web-based! Once installed, you can query your database from a web browser and still enjoy a native application feeling (it's almost entirely based on AJAX, so page reloads are rare).

# User Management
  ![Login](/doc/img/login.png)

Cyclop does not manage users - it passes authorization and authentication to Cassandra. Once the Cassandra session has been opened, it's stored in the HTTP session, and that's it. From now on, each query will be passed to Cassandra over its active session, and the result will be successful or not - based on access rights defined in Cassandra for this particular user.

Providing support for persistent data like query history gets tricky if there is no such thing as a user.

As you might notice, the UUID is a solution to all our problems; this time, it helped too! Cyclop generates random cookie based on UUID and stores it in a browser.. We do not recognize the user itself but the browser. Of curse, a valid Cassandra session is always required, so it's not possible that an unauthorized user could access restricted data (like query history) only because he has access to the browser or "knows" the cookie value; he would have to login in the first place.  

User Preferences cover the number of rows in the result table, import settings, or button state. Those are stored in the browser as a cookie in JSON format. Firstly, there is no security issue because it's not sensitive data. Secondly, we can access it directly from Java Script.

The login form contains two security features that should prevent brute force attacks: 
* after each failed login, the login page blocks for some time before the next login is possible. Also, each failed attempt increases blocking time - until a defined maximal value is reached. We are blocking not only this particular browser/client, but all login requests - it's a global lock. It opens Cyclop for DOS attacks but simultaneously increases brute force resistance. It is not a big issue if Cyclop stops responding because it's not meant to be used by a large number of customers
* login form supports Captcha, but it's not always active - the first failed login activates it for a few minutes

# Query Editor
#### Query Completion
* completion is supported for almost the whole CQL 3 syntax
* Completion Hint shows all possible valid keywords for the actual query position. Tables, keyspaces, and columns are grouped and sorted. Groups are also highlighted with different font color
![CQL Completion](/doc/img/cmp_colors.png)
* if the keyspace has been set in the previous query ("use cqldemo" in the screenshot below), the completion for the following queries
will be narrowed to tables from this keyspace, assuming that keyspace is not explicitly provided in a query
![CQL Completion](/doc/img/cmp_tables_from_global_keyspace.png)
* completion contains only tables that belong to the keyspace that has been provided in the current query
![CQL Completion](/doc/img/cmp_cqldemo_tables.png)
* completion contains only columns of a table that has been provided in the current query
![CQL Completion](/doc/img/cmp_table_columns.png)
* query syntax help has been copied from Cassandra's documentation. It is decorated with color highlighting
matching Completion Hint colors
![CQL Syntax Help](/doc/img/cql_syntax_help.png)

#### Keyboard Navigation
* Enter - confirms currently highlighted completion
* Tab - next completion value
* Ctrl+Enter - executes query
* ESC - cancel completion
* Ctrl+J - jump to page top

# Query Results Table
* results table supports two layouts: horizontal and vertical
* horizontal layout is known from traditional databases
![Results Table](/doc/img/results_table_hor.png)
* vertical layout is reversed compared to traditional SQL editors - rows are displayed horizontally,
and columns vertically. You will switch between rows when scrolling the page from left to right. Scrolling from top to bottom
shows follow-up columns
![Results Table](/doc/img/results_table_vert.png)
* table header for each row displays the partition key, assuming that the query returns it
* long text is trimmed to fit into a table cell. Such a cell has a blue icon in the top left corner. Clicking on it opens a pop-up containing the whole text
![Large Content](/doc/img/large_content.png)

# Query History
![Query History Full](/doc/img/history_full.png)
* history contains the last 500 queries that have been successfully executed from this particular browser (we recognize users based on a persistent cookie)
* Each entry in history contains the query itself, the runtime, and the response size
* next to the query, there is a blue icon. Clicking on it will trigger a redirect to the editor, and paste the query into it so that you can execute it again

#### History Filtering
* filter supports live updates - you get results while typing. Just remember that words shorter than three characters will be ignored
* multiple keywords are joined by OR. It means that the filter result will contain queries with at least one keyword
* you can specify multiple keywords in the filter. Is this case, the top of the filtered history will contain queries with the most hits. It builds groups, like queries with four hits, then three, and at the end with a single hit. The queries within those groups are sorted by execution time
![Query History Filter](/doc/img/history_filter.png)
* pressing Enter resets filter, you can also click on the "clean" icon

#### Data on the Server
The history is stored on a server in a file: [fileStore.folder]\QueryHistory-[User-UUID].json. The file itself contains serialized history in JSON form. The solution is also secure so you can use Cyclop from any computer without restrictions. A random cookie is the only information stored in the browser - but this does not matter because history can be viewed only by authenticated users.

# CSV Export
The query result can be exported to a CSV file. You can alter its format through a configuration file.

# Import
![Query Bookmark](/doc/img/import.png)
It's meant to import files containing CQL queries separated by ;\n. A single query can be spread over multiple lines. The import results are displayed in a table containing every single query, runtime, and eventually an error - in this case, the row is highlighted in red. You can also specify a few options, so that script execution will break (or not) after the first error, or executed queries can be included in query history or parallel import. The last option will divide the import file into chunks and run each in a separate thread - by default six. Be aware that queries will be executed in an unspecified order.

Import also has a few limitations:
* import script and results table have to fit into memory
* each query will be executed as a separate Cassandra call so that we can precisely point out errors and measure execution time. On the other hand, side it causes latencies
* import script does not support comments

# Requirements
* Java 8
* Cassandra v1.2 or above (tested with 1.2 and 2.0, and 2.1)
* only CQL 3 is supported
* CLI is NOT supported
* Tomcat v7 or another v3.x web container

# Technologies
* web app - v3.x
* maven - v3.x
* spring - v4.x
* wicket - v6.x
* wicket-auth-roles - v6.x
* bootstrap - v3.x (theme: cyborg from bootswatch)
* jQuery UI - v1.10.x
* kaptcha 0.0.9
* cassandra-driver-core - v2.x
* slf4j/logback - v1.7.x
* hibernate validator - v5.x
* guava - v16.x (Cassandra 2.0 does not work with v17)

# Installation - Cyclop 2.x for Java 8 
1. Install Java 8 and Maven 3
2. Download last release: `https://github.com/maciejmiklas/cyclop/releases/latest`
3. Edit property file: `cyclop/cyclop-webapp/src/main/resources/cyclop.properties` and set connection settings for
Cassandra:

    ``` properties
    cassandra.hosts: localhost
    cassandra.port: 9042
    cassandra.useSsl: false
    cassandra.timeoutMilis: 3600000
    ```
    You can also overwrite each property from `cyclop.properties` by setting it as a jvm parameter. For example, to connect to different Cassandra host set:`-Dcassandra.hosts=server1,server2`. It gives you a simple possibility to change properties after the war file has been assembled.
    
4. Optionally change logger settings by editing `logback.xml`. By default, it logs in into `/var/lib/tomcat7/logs/cyclop-${time}.log`
5. Build war file: 
    
    ``` sh
	cd cyclop
	cd cyclop-wicket-components
	mvn install
	cd ..
	cd cyclop-webapp
	mvn package
    ```
6. Drop war file into tomcat

    The created war can connect only to one Cassandra cluster, in order to serve multiple clusters from one Tomcat
you have to deploy few cyclop war archives, each one with different `cassandra.hosts` value


# Installation - Cyclop 1.x for Java 7
1. Install Java 7 and Maven 3
2. Download the last release: `https://github.com/maciejmiklas/cyclop/releases/tag/v1.4.2`
3. Edit property file: `cyclop/src/main/resources/cyclop.properties` and set connection settings for
Cassandra:

    ``` properties
    cassandra.hosts: localhost
    cassandra.port: 9042
    cassandra.useSsl: false
    cassandra.timeoutMilis: 3600000
    ```
    You can also overwrite each property from `cyclop.properties` by setting it as jvm parameter. For example, to connect to different Cassandra host set:`-Dcassandra.hosts=server1,server2`. This gives you simple possibility to change properties after the war file has been assembled.
    
4. Optionally change logger settings by editing `logback.xml`. By default it logs in into `/var/lib/tomcat7/logs/cyclop-${time}.log`
5. Build war file: `mvn package`
6. Drop war file into tomcat

    The created war can connect only to one Cassandra cluster, in order to serve multiple clusters from one Tomcat
you have to deploy few cyclop war archives, each one with different `cassandra.hosts` value
