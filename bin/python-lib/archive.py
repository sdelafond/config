import os, re, sys, zipfile
from execute import execute

pjoin = os.path.join

incomingDir = '/data/mp3s/incoming'
easytagDir = pjoin(incomingDir, 'easytag')
archiveDir = pjoin(incomingDir, 'archive')

class Archive:
  """Base abstract class."""
  def __init__(self, fileName):
    self.fileName = fileName
    self.content  = []
    self.initContent()

  def getContent(self):
    return self.content

  def extract(self):
    directory = pjoin(incomingDir, self.fileName[:-4])
    execute('rm -fr "%s"' % directory)
    pwd = os.getcwd()
    try:
      os.mkdir(directory)
    except OSError:
      pass
    os.chdir(directory)
    execute(self.extractCommand % self.fileName)
    os.chdir(pwd)
    os.rename(self.fileName, pjoin(archiveDir, os.path.basename(self.fileName)))

    list1 = os.listdir(directory)
    if len(list1) == 1 and os.path.isdir(pjoin(directory, list1[0])):
      actualDir = pjoin(directory, list1[0])
      tmpDir = directory + '.bak'
      os.rename(actualDir, tmpDir)
      os.rmdir(directory)
      os.rename(tmpDir, directory)

    execute('chmod u+rw -R "%s"' % directory)
    return directory
  
  def isProtected(self):
    pass

class TarArchive(Archive):
  def __init__(self, fileName):
    Archive.__init__(self, fileName)
    self.extractCommand = 'tar xvf "%s"'
    
  def initContent(self):
    for line in execute('tar tvf "%s"' % self.fileName):
      self.content.append(line.split()[5])

class RarArchive(Archive):
  def __init__(self, fileName):
    Archive.__init__(self, fileName)
    self.extractCommand = 'rar x "%s"'
    
  def initContent(self):
    for line in execute('rar l "%s"' % self.fileName):
      self.content.append(line)
    
class AceArchive(Archive):
  def __init__(self, fileName):
    Archive.__init__(self, fileName)
    self.extractCommand = 'unace e "%s"'
    
  def initContent(self):
    for line in execute('unace l "%s"' % self.fileName):
      if line.count('%'):
        self.content.append(''.join(line.split()[4:]))
      
class ZipArchive(Archive):
  def __init__(self, fileName):
    Archive.__init__(self, fileName)
    self.extractCommand = 'unzip "%s"'
  
  def initContent(self):
    zipObject = zipfile.ZipFile(self.fileName)
    for member in zipObject.infolist():
      self.content.append(member.filename)

signatures = { 'RAR archive data':RarArchive,
               'ACE archive data':AceArchive,
               'Zip archive data':ZipArchive,
               'tar archive':TarArchive }

def getFileType(file):
  output = execute('file "%s"' % file)[0]

  for sig in signatures:
    if output.count(sig):
      return signatures[sig]

  return None

class TypeException(Exception):
  pass

def getArchiveObject(fileName):
  """Factory function."""
  
  myType = getFileType(fileName)
  if callable(myType):
    return myType(fileName)
  else:
    raise TypeException('"%s" is of no known type' % fileName)

if __name__ == '__main__':
  for fileName in os.listdir(incomingDir):
    fileName = pjoin(incomingDir, fileName)
    if os.path.isfile(fileName):
      try:
        t = getArchiveObject(fileName)
        #  print t.getContent()
        print t.extract()
      except TypeException, e:
        pass
