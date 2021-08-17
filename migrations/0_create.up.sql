create table store (
  id uuid not null,
  name text not null,
  primary key (id)
);

create table beer (
  id uuid not null,
  name text not null,
  ibu int not null,
  primary key (id)
);
