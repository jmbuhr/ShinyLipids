# Server function -------------------------------------------------------------------------------------------------
function(input, output, session) {




    # End -------------------------------------------------------------------------------------------------------------
    # End session when window is closed
    session$onSessionEnded(stopApp)
}
