// Copyright 2016 the V8 project authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

InspectorTest.sendCommand("Runtime.evaluate", { "expression": "({p1: {a:1}, p2: {b:'foo', bb:'bar'}})" }, callbackEvaluate);

function callbackEvaluate(result)
{
  InspectorTest.sendCommand("Runtime.getProperties", { "objectId": result.result.result.objectId, "ownProperties": true }, callbackGetProperties.bind(null, false));
  InspectorTest.sendCommand("Runtime.getProperties", { "objectId": result.result.result.objectId, "ownProperties": true, "generatePreview": true }, callbackGetProperties.bind(null, true));
}

function callbackGetProperties(completeTest, result)
{
  for (var property of result.result.result) {
    if (!property.value || property.name === "__proto__")
      continue;
    if (property.value.preview)
      InspectorTest.log(property.name + " : " + JSON.stringify(property.value.preview, null, 4));
    else
      InspectorTest.log(property.name + " : " + property.value.description);
  }
  if (completeTest)
    InspectorTest.completeTest();
}
