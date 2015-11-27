import requests
import json
import sys
from os import walk

URL = "https://teaching.doc.ic.ac.uk/wacc_compiler/run.cgi"
#options = [('options[]', opt) for opt in sys.argv[1:-1]]
#source = open(sys.argv[-1])
#r = requests.post(URL, files={'testfile': ('test.wacc', source)}, data=[('stdin', ''), ('run', '')] + options)

def getOutput(filepath, opt):
  source = open(filepath)
  options = [('options[]', opt)]
  r = requests.post(URL, files={'testfile': ('test.wacc', source)}, data=[('stdin', ''), ('run', '')] + options)
  resp = json.loads(r.text)
  return resp['compiler_out']


def genPair(filepath):
  exeOut = getOutput(filepath, "-x")
  sep = "=" * 58
  outputText = find_between(exeOut, sep, sep)
  exitCode   = getExit(exeOut)
  #return filepath, filter(None,outputText.split('\n')), exitCode)
  return json.dumps({filepath : [exitCode] + filter(None, outputText.split('\n'))})

def find_between(s, first, last):
    try:
        start = s.index( first ) + len( first )
        end = s.index( last, start )
        return s[start + 1 : end]
    except ValueError:
        return "NOT FOUND. 1: " + str(first) + ", 2: " + str(last)

def getExit(msg):
  try:
    return msg.split("The exit code is ",1)[1][0]
  except IndexError:
    return "Message: " + msg + " has no exit code"

def getAllFiles(root):
  files = []
  for (dirpath, _, filenames) in walk(root):
    if dirpath.split("/")[-1] != "print":
      continue
    res = map (lambda x: dirpath +"/" + x, filenames)
    files.extend(res)

  return files

#test = map (lambda x : genPair(x), getAllFiles("../../wacc_examples/valid"))
#for f in getAllFiles("../../wacc_examples/valid"):
#  print genPair(f)
#print getAllFiles("../../wacc_examples/valid/")
data = []
data.append(genPair("../../wacc_examples/valid/IO/print/printBool.wacc"))
data.append(genPair("../../wacc_examples/valid/IO/print/printInt.wacc"))
print json.dumps(data)
with open('output.txt', 'w') as outFile:
  json.dump(data,outFile)
