
USE CqlDemo;

INSERT INTO MyBooks 
(id,
title,
genre,
publishDate,
description,
authors,pages,price,
paperType) VALUES (
	1ff18f41-cfb8-45ff-9e89-fb20f95ffc5d,'XML Developers Guide',
	'Computer','2000-10-01','An in-depth look at creating applications with XML.',
	{'Gambardella, Matthew','Marcin Miklas','Fryderyk Zajac','Anna Zajac'},112291,
	{'D':3.85,'E':4.11,'F':4.00},'white and soft');

INSERT INTO MyBooks (id,title,pages,price) VALUES (644556a9-651b-45b3-bd01-cc807c64bd4f,'some text in title ;)',112291,{'DE':4,'EU':34});

	INSERT INTO MyBooks (id,title,pages,price) VALUES (38710416-6253-4a77-a31c-6429f16f3837,'just title..... NR 2',112291,{'DE':4,'EU':4});
	
INSERT INTO MyBooks (id,
	title,
		pages,price) VALUES (
				c746c90c-94dc-45dc-9b47-e410e46a0e61,'just title
								..... urrr...',112291,{'DE':44,'EU':343});
INSERT INTO MyBooks (id,title,
		pages,price) VALUES (6ceb1c47-0955-4654-80d4-5230b88467d2,'just title.....',112291,{'DE':4,'EU':34});
INSERT INTO MyBooks (id,
                title,pages,price) VALUES (4d8c695c-a8e9-4f74-8462-1def6ec075d3,'just dasd title.....',112291,{'DE':4,'EU':344});
INSERT INTO MyBooks (id,title,pages,            price) VALUES (e1390b2e-1393-490b-aa6e-88874ac1fc88,            'just title. wed dwe....',112291,           {'DE':4,'EU':324});


