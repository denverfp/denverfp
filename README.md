# denverfp

Welcome to the DenverFP website repository!

This is a Haskell application using the Yesod web framework.

# Setup:

1. Install the [`stack`](https://docs.haskellstack.org/en/stable/README/) build tool for Haskell projects.
2. Run `stack setup` to install the compiler.
3. Setup Postgresql according to the instructions below.
3. `stack test` will build all the dependencies and run the test suite.

# Database Setup

The application uses PostgreSQL.
To initialize the database, install PostgreSQL on your system.

## Ubuntu:

1. Run `sudo su postgres` to enter a shell as Postgres user.
2. Run `createdb denverfp` to create the development database.
3. Run `createdb denverfp_test` to create the test database.
4. Run `createuser denverfp` to create the dev user
5. Run `psql` to enter a Postgres shell
6. Do `ALTER USER denverfp WITH PASSWORD 'denverfp'` to set the password for the user
