# H-API

This is a sample project in order to familiraze myself with the haskell backend ecosystem (PostgreSQL + Persistent + Esqueleto + Servant + Yesod?) . The ultimate goal is to build an API and host it somewhere.

## Current State

Currently, we have a complete mapping of the tables and types from the DB. The next step would be designing and implementing the endpoints.

## Testing

Currently, all the configuration needed in order to populate the testing DB is found in a `.env` file. See [.env.example](./.env.example) in order to create your own file.

Testing is a bit different, although running the test suit is still done with:

```bash
stack test
```

You can also pass parameters to it in order to `flush` or re`populate` (random data) the database:

```bash
stack test --test-arguments [ populate | flush ]
```

## TODO

- Design and implement the endpoints
- Design and implement the test suit for those endpoints
- Upload the ER-Diagram of the database tables
- Fix the orphan instance of internal module: create a new module for the eval typeclass and TH stuff, and move the TH module into the Domain module.
