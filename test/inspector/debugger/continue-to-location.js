// Copyright 2016 the V8 project authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

InspectorTest.evaluateInPage(
`function statementsExample()
{
  var self = arguments.callee;

  debugger;

  self.step = 1;

  self.step = 2;

  void [
    self.step = 3,
    self.step = 4,
    self.step = 5,
    self.step = 6
  ];

  self.step = 7;
}`);

var scenario = [
  // requested line number, expected control parameter 'step', expected line number
  [ 8, 1, 8 ],
  [ 8, 1, 8 ],
  [ 12, 6, 17 ],
  [ 13, 6, 17 ],
  [ 17, 6, 17 ],
  [ 17, 6, 17 ],
];

InspectorTest.sendCommand("Debugger.enable", {});

InspectorTest.sendCommand("Runtime.evaluate", { "expression": "statementsExample" }, callbackEvalFunctionObject);

function callbackEvalFunctionObject(response)
{
  var functionObjectId = response.result.result.objectId;
  InspectorTest.sendCommand("Runtime.getProperties", { objectId: functionObjectId }, callbackFunctionDetails);
}

function callbackFunctionDetails(response)
{
  var result = response.result;
  var scriptId;
  for (var prop of result.internalProperties) {
    if (prop.name === "[[FunctionLocation]]")
      scriptId = prop.value.value.scriptId;
  }

  nextScenarioStep(0);

  function nextScenarioStep(pos)
  {
    if (pos < scenario.length)
      gotoSinglePassChain(scriptId, scenario[pos][0], scenario[pos][1], scenario[pos][2], nextScenarioStep.bind(this, pos + 1));
    else
      InspectorTest.completeTest();
  }
}

function gotoSinglePassChain(scriptId, lineNumber, expectedResult, expectedLineNumber, next)
{
  InspectorTest.eventHandler["Debugger.paused"] = handleDebuggerPausedOne;

  InspectorTest.sendCommand("Runtime.evaluate", { "expression": "setTimeout(statementsExample, 0)" });

  function handleDebuggerPausedOne(messageObject)
  {
    InspectorTest.log("Paused on debugger statement");

    InspectorTest.eventHandler["Debugger.paused"] = handleDebuggerPausedTwo;

    InspectorTest.sendCommand("Debugger.continueToLocation", { location: { scriptId: scriptId, lineNumber: lineNumber, columnNumber: 0} }, logContinueToLocation);

    function logContinueToLocation(response)
    {
      if (response.error) {
        InspectorTest.log("Failed to execute continueToLocation " + JSON.stringify(response.error));
        InspectorTest.completeTest();
      }
    }
  }
  function handleDebuggerPausedTwo(messageObject)
  {
    InspectorTest.log("Paused after continueToLocation");
    var actualLineNumber = messageObject.params.callFrames[0].location.lineNumber;

    InspectorTest.log("Stopped on line " + actualLineNumber + ", expected " + expectedLineNumber + ", requested " + lineNumber + ", (0-based numbers).");

    InspectorTest.eventHandler["Debugger.paused"] = handleDebuggerPausedUnexpected;

    InspectorTest.sendCommand("Runtime.evaluate", { "expression": "statementsExample.step" }, callbackStepEvaluate);
  }

  function callbackStepEvaluate(response)
  {
    var resultValue = response.result.result.value;
    InspectorTest.log("Control parameter 'step' calculation result: " + resultValue + ", expected: " + expectedResult);
    InspectorTest.log(resultValue === expectedResult ? "SUCCESS" : "FAIL");
    InspectorTest.sendCommand("Debugger.resume", { });
    next();
  }

  function handleDebuggerPausedUnexpected(messageObject)
  {
    InspectorTest.log("Unexpected debugger pause");
    InspectorTest.completeTest();
  }
}
