import md5, os

chunkSize = 1 * 1024 * 1024 # 1M

def readChunks(fileName, chunkSize, functions):
  """On each chunk read, we run every function contained in
     the 'functions' sequence"""
  fileSize = os.stat(fileName).st_size
  
  fullChunks, remainder = divmod(fileSize, chunkSize)

  fh = open(fileName)

  for i in range(fullChunks):
    bytes = fh.read(chunkSize)
    for function in functions:
      apply(function, [ bytes ])

  bytes = fh.read(remainder)
  for function in functions:
    apply(function, [ bytes ])

  fh.close()

def md5Digest(deviceObject, chunkSize = chunkSize):
  myMD5 = md5.new()
  readChunks(deviceObject, chunkSize, [ myMD5.update ])
  return myMD5.hexdigest()
