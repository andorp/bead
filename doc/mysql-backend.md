# MySQL backend

## Build

BE-AD can be built with MySQL support by passing the MySQL flag at the configuration stage.

    cabal clean
    cabal configure --flags "MySQL"
    cabal build

## Prerequisites

Before starting up, the server will need an installed and configured MySQL
server, a user, and a database. In absence of any of these, the server will
refuse to start.

## Configuration

Once BE-AD was built and installed successfully, the default configuration
file will need to be changed by uncommenting the `mysql-config` section. There,
the names of the contained fields are self explanatory -- note that `username` and
`password` refer to the MySQL user.

    # When bead is compiled with MySQL support
    mysql-config:
       database: 'bead-test-db'
       hostname: 'mysql.server.com'
       port: 3306
       username: 'bead'
       password: 'secret'

## Testing

Testing with the MySQL backend is limited at the moment. For running the test suite,
the `defaultConfig` constant (or record) shall be changed in the `Bead.Persistence.SQL.MySQL`
module to set MySQL server and database to be used.
Then rebuild the package and run the tests with `cabal test`.

## Docker

Note that the Docker image incorporates a pre-configured MySQL installation, so
after launching the container, it is possible to bring it up and it could be used
for development. The contents of `defaultConfig` in the image are already pointing
to this MySQL instance.

For further information please check `./docker/README.md` file.
