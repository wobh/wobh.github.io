---
layout: post
title: "SQL SELECT and ActiveRecord method chaining"
date: "2022-08-28T11:00"
tags: 
  - ruby-on-rails
  - activerecord
  - sql
---

I've wanted to write about various weirdnesses in SQL's `SELECT`
statement, how you can only use certain aliases, and derived columns
in certain contexts, and how this might be effected by the "execution
order" of the query. However, this is a pretty excellent blog post
that well introduces the topic:

-   <https://www.essentialsql.com/order-of-execution-in-sql-explained/>

Which means, all I really need to talk about here, is how I use this
to help me write better ActiveRecord expressions.

<!-- more -->

Firstly, I understand, that by "execution order", we're really only
considering a "logical execution order" or maybe a "gramatical
execution order" as SQL statements may be processed in any old way the
implementation considers optimal. 

The only places where the I can find `SELECT` execution order
documented is in Microsoft's T-SQL, and in PostgreSQL's documentation,
but they are mostly the same.

-   **T-SQL:** <https://docs.microsoft.com/en-us/sql/t-sql/queries/select-transact-sql?view=sql-server-ver15#logical-processing-order-of-the-select-statement>
-   **PostgreSQL:** <https://www.postgresql.org/docs/12/sql-select.html>

However the documentation for MySQL and SQLite do talk about the
impacts of the various clauses, so they shouldn't be neglected, if you
use those systems.

-   **MySQL:** <https://dev.mysql.com/doc/refman/8.0/en/select.html>
-   **SQLite:** <https://sqlite.org/lang_select.html>

Now the constraint I keep forgetting about is that derived columns and
aliases in the `SELECT` clause may ONLY be referenced in `ORDER
BY`. As pointed out in the blog post, it's helpful to keep the
"execution order" in mind.

In Ruby On Rails (or anywhere ActiveRecord is used with a SQL
database) I can write a chain of ActiveRecord methods in the following
order to help me write correct queries. Given ActiveRecord model
`Foo`:

```
Foo.       # FROM
  joins(). # includes, merge
  where().
  group().
  having().
  select().
  distinct.
  order().
  limit()
```

In this way I find it much easier to remember and observe the column
constraint.

Lastly, lets just appreciate the power, nuance, and complexity of
SQL's `SELECT` statement. The idea of SQL was to translate relational
algebra into something English reading and writing people could more
easily read and understand, write and execute with some degree of
confidence, hence it's COBOL-like syntax.
