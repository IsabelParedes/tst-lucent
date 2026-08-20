//
// This file is subject to the terms and conditions defined in
// the file 'LICENSE', which is part of this source code.
//

(function() {

    /**
     * Types of simulator messages sent through sockets
     * @enum {string} 
     **/
    const SIMULATOR_MESSAGE = {
        c2s: {
            getProtocolVersion: "get protocol version", 
            getRunSets: "get run sets", 
            getLauncherConfig: "get launcher config", 
            updateStoringDir: "update storing dir",
            addSimulatorConfig: "add simulator config",
            updateSimulatorConfig: "update simulator config",
            removeSimulatorConfig: "remove simulator config",
            listFiles: "list files",
            importLauncherConfig: "import launcher config",
            getOptimDescrs: "get optim descrs", 
        },
        s2c: {
            storingDirUpdate: "storing dir update",
            simulatorConfigAdd: "simulator config add",
            simulatorConfigUpdate: "simulator config update",
            simulatorConfigRemove: "simulator config remove",
            launcherConfigImport: "launcher config import"
        }
    }

    /**
     * Types of simulation messages sent through sockets
     * @enum {string} 
     **/
    const SIMULATION_MESSAGE = {
        c2s: {
            attachSet: "attach set", 
            submitAction: "submit action", 
            getOptimList: "get optim list", 
            startOptim: "start optim", 
            stopOptim: "stop optim", 
            storeBinFile: "store bin file", 
            tellY: "tell Y",
            runAction: "run action", 
            getRuns: "get runs", 
            addRuns: "add runs", 
            searchMustacheInputs: "search mustache inputs",
            setRunSimulator: "set run simulator", 
            loginPasswordNeeded: "login password needed",
            addLoginPassword: "add login password"
        },
        s2c: {
            initRuns: "init runs", 
            runsAdd: "runs add", 
            runSimulatorSet: "run simulator set", 
            launcherEvent: "launcher event",
            askYEvent: "askY event"
        }
    }

    let simulatorsSocket;
    let simulationsSocket;
    let launcherUrl;
    let socketConnected = undefined;

    const opts = { 'reconnectionAttemps' : 'Infinity', 'timeout' : 10000, 'transports' : ['websocket'] };

	// Register a client-side function to receive 'connectToSimulationsLauncher' messages which come from Shiny server
    Shiny.addCustomMessageHandler('connectToSimulationsLauncher', function(url) {
        launcherUrl = url
        const simulatorsUrl = url + '/simulators';
        // Retrieve a socket instance connected to launcher server, specifying a namespace called 'simulators'
        simulatorsSocket = io(simulatorsUrl, opts);
        simulatorsSocket.on("disconnect", _attemptNumber => {
            setConnected(false);
        })
        simulatorsSocket.on("connect", _attemptNumber => {
            setConnected(true);
        })
        simulatorsSocket = simulatorsSocket.connect();
    });

	// Register a client-side function to receive 'disconnectFromSimulationsLauncher' messages which come from Shiny server
    Shiny.addCustomMessageHandler('disconnectFromSimulationsLauncher', function(args) {
        if (simulatorsSocket) {
            simulatorsSocket.disconnect();
        }
    });

    function setConnected(connected) {
        socketConnected = connected;

        if (Shiny.setInputValue && simulatorsSocket) {
            if (connected) {
                // While waiting for the response to be received, send '-1' as protocol version
                // Note: strange behaviour, 'id' is not as expected (it seems shiny 'ns' function removes one of the 'importDOE')
                Shiny.setInputValue('nav-menuImport-importDOE-launcherProtocolVersion', -1, {priority: "event"});

                simulatorsSocket.emit(SIMULATOR_MESSAGE.c2s.getProtocolVersion, "", function(protocolVersion) {
                    Shiny.setInputValue('nav-menuImport-importDOE-launcherProtocolVersion', protocolVersion, {priority: "event"});
                });
            }
            else {
                Shiny.setInputValue('nav-menuImport-importDOE-launcherProtocolVersion', 0, {priority: "event"});
            }
        }
    }
    $(document).on("shiny:connected", function() {
        setConnected(socketConnected);
    });
	
	// Register a client-side function to receive 'get optim descrs' messages which come from Shiny server
    Shiny.addCustomMessageHandler('getOptimDescrs', function(args) {
        if (simulatorsSocket && !simulatorsSocket.connected) {
            simulatorsSocket.connect();
        }
        // Send a 'get optim descrs' message to the launcher server,
        // providing a callback function, in order to retrieve descriptions of optimizers that can be used for the given problem definition.
        simulatorsSocket.emit(SIMULATOR_MESSAGE.c2s.getOptimDescrs, args.optimPbDef, function(success, optimDescrs) {
            Shiny.setInputValue(args.optimDescrsInputId, 
                { success: success, results: JSON.stringify(optimDescrs) }
            );
        });
    });


	// Register a client-side function to receive 'get optim descrs' messages which come from Shiny server
    Shiny.addCustomMessageHandler('attachSet', function(setName) {
        // Retrieve a socket instance connected to launcher server, specifying a namespace called 'simulations'
        const simulationsUrl = launcherUrl + '/simulations';
        simulationsSocket = io.connect(simulationsUrl, opts);

        simulationsSocket.emit(
            SIMULATION_MESSAGE.c2s.attachSet,
            setName, 
            function() {
                // Register a client-side function to receive 'Cancel' messages which come from Shiny server
                Shiny.addCustomMessageHandler('Cancel', function(args) {
                    // Send a 'remove selection' message to the launcher server
                    simulationsSocket.emit(SIMULATION_MESSAGE.c2s.runAction,  {
                        actionId: "Cancel",
                        runIds: args.runIds
                    });
                });
          
                // Send a 'search mustache inputs' message to the launcher server,
                // providing a callback function, in order to retrieve input variable names for given simulator ids (or for all simulators if ids is empty).
                simulationsSocket.emit(SIMULATION_MESSAGE.c2s.searchMustacheInputs, [], function(mustacheSet) {
                    Shiny.setInputValue('nav-menuImport-importDOE-confSimulator-simulatorsInputs',
                    mustacheSet, {priority: "event"});
                });
                
                // Register a client-side function to receive 'loginPasswordNeeded' messages which come from Shiny server
                Shiny.addCustomMessageHandler('loginPasswordNeeded', function(args) {
                    Shiny.setInputValue(args.neededInputId, null);
                    //  Send a message to know if a login/password must be asked to the user for simulator 'args.simulatorId'
                    simulationsSocket.emit(SIMULATION_MESSAGE.c2s.loginPasswordNeeded, args.logPwdNeededArgs, function(success, needed) {
                        Shiny.setInputValue(args.neededInputId, 
                            { success: success, answer: needed }
                        );
                    });
                });
            
                // Register a client-side function to receive 'addLoginPassword' messages which come from Shiny server
                Shiny.addCustomMessageHandler('addLoginPassword', function(args) {
                    // Send a message to try a login/password
                    simulationsSocket.emit(SIMULATION_MESSAGE.c2s.addLoginPassword, args.logPwdArgs, function(success, validLoginPwd) {
                        Shiny.setInputValue(args.validLoginPwdInputId, 
                            { success: success, answer: validLoginPwd }
                        );
                    });
                });
            
                // Listen event sent by the launcher server
                simulationsSocket.on(SIMULATION_MESSAGE.s2c.askYEvent, function (askYEvent) {
                    // Forward 'askY event' to Shiny through a reactive input value
                    // @ts-ignore
                    Shiny.setInputValue("nav-menuImport-importDOE-directOptim-askYEvent", JSON.stringify(askYEvent), {priority: "event"});
                    // dataframe are flattened by Shiny => workaround, stringify dataframe https://github.com/rstudio/shiny/issues/1098#issuecomment-216579034
                });

                // Register a client-side function to receive 'optimAction' messages which come from Shiny server
                // @ts-ignore
                Shiny.addCustomMessageHandler("optimAction", function(args) {
                    preproArgs = jsonPreProcess(args);
                    // Send 'startOptim' messages to the launcher server
                    simulationsSocket.emit(SIMULATION_MESSAGE.c2s.startOptim, preproArgs, (success, results) => {
                        console.log("success:", success, " results:", results);
                        // @ts-ignore
                        Shiny.setInputValue(args.optimResultsInputId, 
                            { success: success, results: JSON.stringify(results) }, 
                            { priority: "event" }
                        );
                    });
                });

                // Register a client-side function to receive 'getOptimList' messages which come from Shiny server
                // @ts-ignore
                Shiny.addCustomMessageHandler("getOptimList", function(args) {
                    // Send 'getOptimList' messages to the launcher server
                    simulationsSocket.emit(SIMULATION_MESSAGE.c2s.getOptimList, "", function(optimInfoList) {
                        // @ts-ignore
                        Shiny.setInputValue("nav-menuImport-importDOE-confSimulator-optimInfoList", JSON.stringify(optimInfoList), {priority: "event"});
                    });
                });

                // Register a client-side function to receive 'storeBinFile' messages which come from Shiny server
                // @ts-ignore
                Shiny.addCustomMessageHandler("storeBinFile", function(args) {
                    // Send 'storeBinFile' messages to the launcher server
                    simulationsSocket.emit(SIMULATION_MESSAGE.c2s.storeBinFile, args);
                });

                // Register a client-side function to receive 'stopOptim' messages which come from Shiny server
                // @ts-ignore
                Shiny.addCustomMessageHandler("stopOptim", function(optimId) {
                    // Send 'stopOptim' messages to the launcher server
                    simulationsSocket.emit(SIMULATION_MESSAGE.c2s.stopOptim, optimId);
                });

                // Register a client-side function to receive 'tellY' messages which come from Shiny server
                // @ts-ignore
                Shiny.addCustomMessageHandler("tellY", function(args) {
                    // Send 'tellY' messages to the launcher server
                    simulationsSocket.emit(SIMULATION_MESSAGE.c2s.tellY, args);
                });

                // Register a client-side function to receive 'addRuns' messages which come from Shiny server
                // @ts-ignore
                Shiny.addCustomMessageHandler("addRuns", function(args) {
                    if (args.runs.length !== 0) {
                        // Send 'addRuns' message to the launcher server
                        simulationsSocket.emit(SIMULATION_MESSAGE.c2s.addRuns, args.runs, addedRuns => {
                            simulationsSocket.emit(SIMULATION_MESSAGE.c2s.setRunSimulator, {
                                simulatorId: args.simulatorId,
                                runIds: addedRuns.map(r => r.id)
                            });
                            simulationsSocket.emit(SIMULATION_MESSAGE.c2s.runAction, {
                                actionId: "ConfigureLaunchLoad",
                                runIds: addedRuns.map(r => r.id)
                            });
                        });
                    }
                });

                // Register a client-side function to receive 'configureLaunchLoad' messages which come from Shiny server
                // @ts-ignore
                Shiny.addCustomMessageHandler("configureLaunchLoad", function(args) {
                    simulationsSocket.emit(SIMULATION_MESSAGE.c2s.setRunSimulator, {
                        simulatorId: args.simulatorId,
                        runIds: asArray(args.runIds)
                    });
                    simulationsSocket.emit(SIMULATION_MESSAGE.c2s.runAction, {
                        actionId: "ConfigureLaunchLoad",
                        runIds: asArray(args.runIds)
                    });
                });

                // Send 'getRuns' messages to the launcher server
                simulationsSocket.emit(SIMULATION_MESSAGE.c2s.getRuns, "", function(runs) {
                    // @ts-ignore
                    Shiny.setInputValue("nav-menuImport-importDOE-confSimulator-runList", JSON.stringify(runs), {priority: "event"});
                });

                // Listen event sent by the launcher server
                simulationsSocket.on(SIMULATION_MESSAGE.s2c.launcherEvent, function (launcherEvent) {
                    // Forward 'launcher event' to Shiny through a reactive input value
                    Shiny.setInputValue('nav-menuImport-importDOE-confSimulator-launcherEvent',
                                        launcherEvent, {priority: "event"});
                });

                simulationsSocket.on(SIMULATION_MESSAGE.s2c.runsAdd, function (runs) {
                    // Forward 'runs add' to Shiny through a reactive input value
                    Shiny.setInputValue('nav-menuImport-importDOE-confSimulator-runsAdd',
                                        JSON.stringify(runs), {priority: "event"});
                });

                simulationsSocket.on(SIMULATION_MESSAGE.s2c.runSimulatorSet, function (setSimulatorEvent) {
                    // Forward 'run simulator set' to Shiny through a reactive input value
                    Shiny.setInputValue('nav-menuImport-importDOE-confSimulator-runSimulatorSet',
                                        setSimulatorEvent, {priority: "event"});
                });
                
            }
        );
    })

    // Register a client-side function to receive 'retrieveLauncherData' message which come from Shiny server
    Shiny.addCustomMessageHandler('retrieveLauncherData', function(x) {
        if (simulatorsSocket) {
            if (!simulatorsSocket.connected) {
                simulatorsSocket.connect();
            }
            // Send a 'get launcher config' message to the launcher server,
            // providing a callback function, in order to retrieve simulators config.
            simulatorsSocket.emit(SIMULATOR_MESSAGE.c2s.getLauncherConfig, "", function(simulators) {
                Shiny.setInputValue('nav-menuImport-importDOE-confSimulator-simulatorsConfigs',
                simulators, {priority: "event"});
            });
            simulatorsSocket.emit(SIMULATOR_MESSAGE.c2s.getRunSets, "", function(runSets) {
                Shiny.setInputValue('nav-menuImport-importDOE-confSimulator-runsSets',
                runSets, {priority: "event"});
            });
        }
});
    
    function jsonPreProcess(struct) {
        const serialized = JSON.stringify(struct, (_k, v) => {
            if (typeof v === "undefined") {
                return "undefined";
            }
            if (v === Infinity) {
                return "Infinity";
            }
            if (v === -Infinity) {
                return "-Infinity";
            }
            if (typeof v === "object" || typeof v === "string") {
                return v;
            }
            if (isNaN(v)) {
                return "NaN";
            }
            return v;
        });
        return JSON.parse(serialized);
    }

    function r2JsIndex(index) {
        return (index !== null) ? index - 1 : index;
    }

    function js2RIndex(index) {
        return (index !== null) ? index + 1 : index;
    }
    
    function asArray(value) {
        if (!Array.isArray(value)) {
            return [value];
        }
        return value;
    }
    
})(); // Properly isolate your script environment with an IIFE (immediately-invoked function expression)
