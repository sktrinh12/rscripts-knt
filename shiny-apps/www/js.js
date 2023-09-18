Shiny.addCustomMessageHandler("resetFileInputHandler", function(x) {      
				console.log(`${x} test reset file input`);
				alert(`testing - ${x}`);
});

Shiny.addCustomMessageHandler("testmessage",
  function(message) {
    alert(JSON.stringify(message));
  }
);
