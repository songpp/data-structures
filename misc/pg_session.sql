

create table if not exists t1(
  id serial primary key,
  name CHARACTER VARYING(100)
);

insert into t1(name) values('poiro');


select * from t1;