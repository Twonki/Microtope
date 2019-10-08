import cors = require("cors");
import express from "express";
import {factory} from "./logging/ConfigLog4j";
import * as indexRoutes from "./routes/index";

const log = factory.getLogger("index");

const app = express();

let exposedPort: number = 8080; // default exposedPort to listen

log.info("Starting API ... ");

// Configure Express to parse incoming JSON data
app.use( express.json() );
app.use(cors());

/*
* The process args [0] is the node exe, the process args[1] is the file running
* This is a little strange behaviour, but what in node is not?
* This is why the args-array-logic starts with [2]
*/
if (process.argv.length === 2) {
    log.error("Did not find any system args - Shutting down!");
  } else {
    log.debug("Found Systemargs ... ");
    const DBHost: string = process.argv[2];
    const DBUser: string = process.argv[3];
    const DBPwd: string = process.argv[4];
    const DBPort: number = Number.parseInt(process.argv[5], 10);
    const DBName: string = process.argv[6];

    exposedPort = Number.parseInt(process.argv[7], 10);

    log.info("Connecting to Host:" + DBHost + ":" + DBPort);
    log.info(" as User " + DBUser + " with PWD " + "[[REDACTED]]" +  " on DB " + DBName);
    indexRoutes.register(app, DBHost, DBUser, DBPwd, DBPort, DBName );

  }

// start the Express server
app.listen( exposedPort, () => {
    log.info( `server started at http://localhost:${ exposedPort }` );
} );
