DROP TABLE IF EXISTS post;

create table post (
       postid rowid,
       title text,
       content text,
       upvotes int,
       downvotes int
       );
