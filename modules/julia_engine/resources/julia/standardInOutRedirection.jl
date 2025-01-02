# Backup stdout

# Create custom IO buffer
const io_stdout = IOBuffer()
Base.stdout = io_stdout;
redirect_display = TextDisplay(IOContext(io_stdout))
pushdisplay(redirect_display)

const io_stderr = IOBuffer()
Base.stderr = io_stderr;

# Define function to retrieve buffer contents
function get_stdout()
  try  
    return String(take!(copy(io_stdout)))
  catch
    return ""
  end    
end

function get_stderr()
  return String(take!(copy(io_stderr)))
end

function clear_stdout()
  try
    truncate(io_stdout, 0)
  catch
    nothing
  end    
end

function clear_stderr()
  try
    truncate(io_stderr, 0)
  catch
    nothing
  end    
end
