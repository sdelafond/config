import popen2, os, select

# set the path for finding binaries
defaultPath   = [' /bin', '/sbin', '/usr/bin', '/usr/sbin',
                 '/usr/local/bin', '/usr/local/sbin' ]
pathSeparator = ':'
defaultPath   = pathSeparator.join(defaultPath)
try:
  os.environ['PATH'] = '%s%s%s' % (os.environ['PATH'], pathSeparator,
                                   defaultPath)
except KeyError:
  os.environ['PATH'] = defaultPath

# buffer size
size = 100

class ExecutionError(Exception):
  """The Exception class for this module"""
  def __init__(self, value):
    Exception.__init__(self)
    self.value = value
  def __str__(self):
    return str(self.value)

def __readFromFileDescriptor(fd, size = size):
  """Helper function"""
  dataChunk = os.read(fd, size)
  if dataChunk == '':
    raise EOFError
  else:
    return dataChunk

def launch(command, trace = False):
  proc = popen2.Popen3(command, capturestderr = 1)
  
#  proc.tochild.close() # no need to talk to the child

  if trace:
    print "Started: %s" % command
    print "with PID: %i" % proc.pid

  return proc

def wait(proc, trace = 0, ignoreErrors = 0, hideErrors = 0, command=""):

  outfd = proc.fromchild.fileno()
  errfd = proc.childerr.fileno()

  outdata = errdata = []
  outEOF  = errEOF = 0

  while not (outEOF and errEOF):
    ready = select.select( [outfd, errfd], [], [] ) # wait for input
    if outfd in ready[0]: # data on stdout
      try:
        outdata.append(__readFromFileDescriptor(outfd))
      except EOFError:
        outEOF = 1
    if errfd in ready[0]: # data on stderr
      try:
        errdata.append(__readFromFileDescriptor(errfd))
      except EOFError:
        errEOF = 1

  if trace:
    print "Waiting for process to finish"
                
  statusCode = proc.wait()

  if trace:
    print "Done."

  if os.WIFEXITED(statusCode):
    returnCode = os.WEXITSTATUS(statusCode)
  else: # caught a signal
    returnCode = 1
    if trace:
      print "Caught a signal..."
    
  output = ''.join(outdata)
  
  if returnCode:
    message = "Couldn't do: '%s'\n" % command
    message += "Stdout: %s\n" % output
    message += "Stderr: %s\n" % ''.join(errdata)
    if not ignoreErrors:
      if hideErrors:
        print message
      else:
        raise ExecutionError(message)
  else:
    return output.split('\n')[:-1] # return output as an array of lines
#    return output

# the main entry point for that module
def execute(command, trace = 0, ignoreErrors = 0, hideErrors = 0):
  try:
    proc = launch(command,trace)  
  except Exception,e:
    if not hideErrors:
      print e
    raise ExecutionError("launch Failed '%s'" % command)

  return wait(proc,trace,ignoreErrors,hideErrors,command)




if __name__ == "__main__":
  import sys
  print execute(sys.argv[1], trace = 1)
