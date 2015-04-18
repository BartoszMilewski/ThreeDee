{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

import Yesod
import Types
import Diagram

-- Yesod setup

data ThreeDee = ThreeDee

instance Yesod ThreeDee

mkYesod "ThreeDee" [parseRoutes|
    /          HomeR      GET
    /diagram   WorldR   GET
    /transform TransformR POST
|]

boxColor, bodyColor :: String

boxColor   = "#fff"
bodyColor  = "#333"

-- Canvas box
boxSizeX, boxSizeY  :: Int
boxSizeX    = 800
boxSizeY    = 400

-- Home page handler

getHomeR :: Handler Html
getHomeR = defaultLayout $ do
  setTitle "ThreeDee"
  -- HTML
  toWidget [whamlet|
    <div #box>
      <canvas #canvas width=#{boxSizeX} height=#{boxSizeY}> 
         Your browser doesn't support HTML 5
    <p #text>Text
  |]
  -- CSS
  toWidget [cassius|
    #box
      width: #{show boxSizeX}px
      height:#{show boxSizeY}px
      margin-left:auto
      margin-right:auto
    canvas
      background-color:#{boxColor}
    body
      background-color:#{bodyColor}
      color:#fff
      font-family:Arial,Helvetica,sans-serif
      font-size:small
    #text
      text-align:center
  |]
  -- JavaScript
  addScriptRemote "//ajax.googleapis.com/ajax/libs/jquery/1.9.1/jquery.min.js"

  toWidget [julius|
    var dimX = #{toJSON boxSizeX};
    var dimY = #{toJSON boxSizeY};

    var curDiagram = null;
    // worlds have sequence numbers to eliminate updates that come out of order
    var curSeqNum = 0;
    // Mouse position as a proxy for rotation angle
    var lastX = 0;
    var lastY = 0;

    $(document).ready(function() {
        // Get the initial world
        $.getJSON("@{WorldR}", function(myWorld) {
            curDiagram = myWorld.diagram;
            curSeqNum = myWorld.seqNum;
            draw();
        });
        // Mouse drag handler
        $('canvas').mousedown(function(event) {
            var parentOffset = $('canvas').parent().offset();
            lastX = event.pageX - parentOffset.left;
            lastY = event.pageY - parentOffset.top;
            // Subsequent moves
            $(window).mousemove(function(event) {
                var x = event.pageX - parentOffset.left;
                var y = event.pageY - parentOffset.top;
                getNewWorld(x, y);
            });
            // Unbind mousemove handler also when mouse outside of canvas
            $(document).mouseup(function() {
                $(window).unbind("mousemove");
            })
        });
        $('canvas').css('cursor', 'move');
        $('#text').text("Drag the mouse pointer to rotate");
    });
    // Request the update using new mouse position
    // We are sending the old diagram and the "angle"
    function getNewWorld(x, y) {
        // Get new world from server
        var oldWorld = new Object;
        oldWorld.seqNum = curSeqNum;
        oldWorld.delta = new Object;
        // Delta in the mouse movement since last update
        // A proxy for rotation angle
        oldWorld.delta.vx = x - lastX;
        oldWorld.delta.vy = y - lastY;
        oldWorld.diagram = curDiagram;
        $.ajax(
        {
           "data"    : JSON.stringify(oldWorld),
           "type"    : "POST",
           "url"     : "@{TransformR}",
           "success" : onWorldUpdate
        });
    }
    // Handler of updates from the server
    function onWorldUpdate(newWorld) {
        var lag = curSeqNum - newWorld.seqNum;
        if (lag == 0) {
            curDiagram = newWorld.diagram;
            // update "rotation angles"
            lastX += newWorld.delta.vx;
            lastY += newWorld.delta.vy;
            // update sequence number
            curSeqNum = newWorld.seqNum + 1;
            draw();
        } else if (lag < 0)
            alert("Time travel discovered! Current: " + curSeqNum + " New: " + newWorld.seqNum)
    }
    // Draw current diagram
    function draw()
    {
        if (!curDiagram)
            alert("No diagram");
        var canvas = document.getElementById('canvas');
        var ctx = canvas.getContext('2d');
        ctx.clearRect(0, 0, dimX, dimY);
        ctx.font="15px Times Roman";

        // Map point ordinal to point data for edge drawings
        var pointMap = new Object;
        for (var j = 0; j < curDiagram.points.length; j++) {
            var point = curDiagram.points[j];
            pointMap [point.n] = point;
        }
        // Draw edges first
        for (var j = 0; j < curDiagram.edges.length; j++) {
            var edge = curDiagram.edges[j];
            var p1 = pointMap[edge.n1];
            var p2 = pointMap[edge.n2]
            var x1 = p1.x + dimX/2;
            var y1 = -p1.y + dimY/2;
            var x2 = p2.x + dimX/2;
            var y2 = -p2.y + dimY/2;
            ctx.strokeStyle = edge.ecolor;
            ctx.fillStyle = edge.ecolor;
            arrow(ctx, x1, y1, x2, y2);
            ctx.fillText(edge.eLabel, (x1 + x2) / 2 + 4, (y1 + y2) / 2 - 8);
        }
        // Draw points
        for (var j = 0; j < curDiagram.points.length; j++) {
            var point = curDiagram.points[j];
            ctx.fillStyle = point.pcolor;
            var x = point.x + dimX/2;
            var y = -point.y + dimY/2;
            ctx.beginPath(); 
            ctx.arc(x, y, 3, 0, Math.PI * 2, true); 
            ctx.fill();
            ctx.fillText(point.pLabel, x + 4, y - 8);
        }
        function arrow(ctx, x1, y1, x2, y2)
        {
            ctx.beginPath();
            ctx.moveTo(x1, y1);
            ctx.lineTo(x2, y2);
            ctx.stroke();
            var vx = x1 - x2;
            var vy = y1 - y2;
            var scale = 17 / Math.sqrt(vx*vx + vy*vy);
            vx *= scale;
            vy *= scale;
            // 0.2 and 0.95 are approximately sin and cos of 0.3
            ctx.lineTo(x2 + vx * 0.95 - vy * 0.2, y2 + vy * 0.95 + vx * 0.2);
            ctx.lineTo(x2 + vx * 0.95 + vy * 0.2, y2 + vy * 0.95 - vx * 0.2);
            ctx.lineTo(x2, y2);
            ctx.fill();
        }
    }
  |]

-- Server

postTransformR :: Handler Value
postTransformR = do
    -- Parse the request body to a data type as a JSON value
    wrld <- requireJsonBody
    returnJson $ transformWorld wrld

transformWorld :: World -> World
transformWorld wrld = wrld { 
      diagram = transformDiagram (delta wrld) (diagram wrld)
    }

getWorldR  :: Handler Value
getWorldR  = returnJson World {
    seqNum = 0
  , delta = Vector2 { vx = 0, vy = 0 }
  , diagram = getDiagram
  }

main :: IO ()
main = warpEnv ThreeDee