CREATE KEYSPACE CqlDemo WITH replication = {'class': 'SimpleStrategy', 'replication_factor' : 1};

USE CqlDemo;

CREATE TABLE MyBooks (
    id UUID PRIMARY KEY,
    title TEXT,
    genre TEXT,
    publishDate TIMESTAMP,
    description TEXT,
    authors SET<TEXT>,
    price MAP<TEXT,DOUBLE>,
    pages INT
);

ALTER TABLE MyBooks ADD paperType varchar;

ALTER TABLE MyBooks ADD dynamicColumn1 varchar;
ALTER TABLE MyBooks ADD dynamicColumn2 varchar;
ALTER TABLE MyBooks ADD dynamicColumn3 varchar;

CREATE INDEX ON MyBooks(title);
CREATE INDEX ON MyBooks(publishDate);
CREATE INDEX ON MyBooks(genre);
CREATE INDEX ON MyBooks(pages);