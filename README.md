
Based on
http://boundary.com/blog/2012/09/13/comparing-go-and-java/. See
performance test scripts here https://github.com/patrick-higgins/go-and-java/blob/master/bench.sh

## Local Setup

### Setup Local Database

```
λ sudo -u postgres createdb erlangdc
λ psql erlangdc
=> create table users (
       id text not null,                                                        
       apikey text not null,
       name text not null,
       email text not null,
       password_hash text not null,
       admin boolean not null default false,
       active boolean not null default true,
       created_at timestamp with time zone not null default localtimestamp,
       updated_at timestamp with time zone not null default localtimestamp
       );
=> insert into users (id, apikey, name, email, password_hash)
       values ('1', 'apikey_value', 'John', 'john@email.com', 'password_hash_value');
```

### Build and Run

```shell
λ make rel
λ ./_rel/bin/erlangdc
```

## Heroku Setup

### Create Heroku App

```shell
λ heroku create --buildpack https://github.com/tsloughter/heroku-buildpack-erlang.git
```

### Setup Database

```shell
λ heroku addons:add heroku-postgresql:dev

λ heroku pg:psql DATABASE_URL
=> create table users (
       id text not null,                                                        
       apikey text not null,
       name text not null,
       email text not null,
       password_hash text not null,
       admin boolean not null default false,
       active boolean not null default true,
       created_at timestamp with time zone not null default localtimestamp,
       updated_at timestamp with time zone not null default localtimestamp
       );
=> insert into users (id, apikey, name, email, password_hash)
       values ('1', 'apikey_value', 'John', 'john@email.com', 'password_hash_value');
```

### Push to Heroku

```shell
λ git push heroku master
```

