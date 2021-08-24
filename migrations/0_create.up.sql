-- todo: use uuid type
create table store (
  id varchar(36) not null,
  name text not null,
  primary key (id)
);

create table beer (
  id varchar(36) not null,
  name text not null,
  ibu int not null,
  primary key (id)
);


create table store_beer (
  id varchar(36) not null,
  store_id varchar(36) not null references store(id),
  beer_id varchar(36) not null references beer(id),
  primary key (id)
);
