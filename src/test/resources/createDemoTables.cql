CREATE KEYSPACE CqlDemo WITH replication = {'class': 'SimpleStrategy', 'replication_factor' : 1};
USE CqlDemo;
CREATE TABLE MyBooks (id UUID PRIMARY KEY,title TEXT,genre TEXT,publishDate TIMESTAMP,description TEXT,authors SET<TEXT>,price MAP<TEXT,DOUBLE>,pages INT);
create table CompoundTest (id uuid, id2 int, id3 text, deesc text, primary key(id, id2, id3));
ALTER TABLE MyBooks ADD paperType varchar;
ALTER TABLE MyBooks ADD dynamicColumn1 varchar;
ALTER TABLE MyBooks ADD dynamicColumn2 varchar;
ALTER TABLE MyBooks ADD dynamicColumn3 varchar;
ALTER TABLE MyBooks ADD idx int;
CREATE INDEX ON MyBooks(description);
CREATE INDEX ON MyBooks(publishDate);
CREATE INDEX ON MyBooks(genre);
CREATE INDEX ON MyBooks(pages);
CREATE INDEX ON CompoundTest(deesc)