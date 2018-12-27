#!/usr/bin/env python3

import sys
import bs4
import re

def splitListAt(lst, cond):
    l = len(lst)
    l2 = zip(lst, range(l))
    points = list(map(lambda x: x[1], filter(lambda x:cond(x[0]), l2)))
    if len(points) == 0:
        return lst
    if(points[0]) == 0:
        points.pop(0)
    out = []
    last_p = 0
    for p in points:
        out.append(lst[last_p:(p)])
        last_p = p
    out.append(lst[p:])
    return out

def splitAt(lst, tagname):
    def cond(e):
        try: 
            return e.name == tagname
        except:
            return False
    return splitListAt(lst, cond)

def bigHeader(txt):
    l = len(txt)
    s = '-'*120 + '\n'
    s += '-'*120 + '\n'
    i = (120-(l+4)) // 2
    s2 = ('-'*i) + '  ' + txt + '  ' + ('-'*i)
    s2 += '-'*(120-len(s2))
    s += s2 + '\n'
    s += '-'*120 + '\n'
    s += '-'*120 + '\n'
    return s

def smallHeader(txt):
    l = len(txt)
    i = (120-(l+4)) // 2
    s = ('-'*i) + '  ' + txt + '  ' + ('-'*i)
    s += '-'*(120-len(s))
    return s

def commentAbove(txt):
    s = ''
    first = True
    for line in txt.split('\n'):
        s += '-- ' + ('|' if first else '') + line + '\n'
        first = False
    return s.strip()

def comment(txt):
    return '\n'.join(map(lambda line: '-- ' + line, txt.split('\n')))

def indent(txt, n=4):
    return "\n".join(map(lambda x: ' '*n + x, txt.split("\n")))

def unTitle(s):
    return s[0].lower() + s[1:]

def title(s):
    return s[0].upper() + s[1:]

def getDescription(h3):
    s = ''
    for e in h3:
        if e is None or e.name == 'h3': continue
        for span in e.findAll('span'):
            span.replaceWith("`"+span.text+"`")
        s += u''.join(e.findAll(text=True))
    s = "\n".join(map(str.strip, s.split("\n")))
    return s

def getParameters(h3):
    out = []
    table = list(filter(lambda x: x.name == 'table', h3))[0]
    first = True
    for tr in table.findAll('tr'):
        if first: first= False; continue
        tds = list(map(lambda x: x.text.strip(), tr.findAll('td')))
        out.append(tds)
    return out

def getReturns(h3):
    return list(map(lambda x: x.text, filter(lambda x: x.name != 'h3', h3)))

def getSee_Also(h3):
    h3 = list(filter(lambda x: x.name != 'h3', h3))
    s = ''
    for e in h3:
        if e is None or e.name == 'h3': continue
        for span in e.findAll('span'):
            span.replaceWith("`"+span.text+"`")
        s += u''.join(e.findAll(text=True)) + '\n'
    return s.strip()

def getMembers(h3):
    out = []
    table = list(filter(lambda x: x.name == 'table', h3))[0]
    first = True
    for tr in table.findAll('tr'):
        if first: first= False; continue
        tds = list(map(lambda x: x.text.replace("\n"," ").strip(), tr.findAll('td')))
        out.append(tds)
    return out

lookup_f = {
    'Description':getDescription,
    'Parameters':getParameters,
    'Returns':getReturns,
    'See Also':getSee_Also,
    'Members':getMembers
}

typeLookup = dict(
    BOOL='Bool',
    UINT8='Word8',
    UINT16='Word16',
    UINT32='Word32',
    UCHAR='Word8'
)

castLookup = dict(
    Bool='fromIntegral',
    Word8='fromIntegral',
    Word16='fromIntegral',
    Word32='fromIntegral'
)

def mkHsType(s):
    if s == '' or s is None:
        return s
    s = s.replace("CY_","")
    s = "".join(map(lambda x: title(x.lower()), s.split("_")))
    return s

def mkHsName(s):
    if s == '' or s is None:
        return s
    return unTitle(mkHsType(s))

doc = bs4.BeautifulSoup(open("Cypress USB-Serial API Documentation_1-shorter.html",'r').read(), features="html.parser")
body = doc.find('body')
exportList = []
for h1 in splitAt(body.contents, 'h1'):
    if h1[0] == '\n': continue
    h1 = list(filter(lambda e: e!= '\n', h1))
    h1Text = re.sub(r'^[0-9]+ *', '', h1[0].text)
    print(bigHeader(h1Text))
    h2s = splitAt(h1, 'h2')
    h2s.pop(0)
    for h2 in h2s:
        h2Text = h2[0].text
        print(smallHeader(str(h2Text)))
        h3s = splitAt(h2, 'h3')
        startText = '\n'.join(map(lambda x: x.text, filter(lambda x: x.name != 'h2', h3s.pop(0))))
        info = {}
        for h3 in h3s: 
            h3Text = h3[0].text
            f = lookup_f[h3Text]
            info[h3Text] = f(h3)
        print(commentAbove(startText))
        if 'Description' in info:
            description = info['Description']
            print("--")
            print(comment(description))
        else: description = None
        if 'See Also' in info:
            print("--\n-- See Also")
            print(comment(info['See Also']).replace(u'•', '*'))
        print("--")
        print("-- " + str(h2Text))
        
        name = re.sub(r'^[0-9\.]+ *', '', h2Text)

        # find out what it is:
        if (('' if description is None else description)+startText).lower().find('enumeration') >= 0:
            typ = "ENUM"
            hsType = mkHsType(name)
            print('{{#enum {} as {} with prefix = "" add prefix = "{}" deriving (Eq, Show)}}'.format(
                name, hsType, unTitle(hsType)))
            exportList.append(name)
        elif (('' if description is None else description)+startText).lower().find('struct') >= 0:
            typ = "STRUCT"
            print("data " + mkHsName(name))
            exportList.append(name + "(..)")
            members = info.get('Members', None)
            dat = []
            if members is not None:
                first = True
                for m in members:
                    pre = "    ," if not first else "    {"
                    first = False
                    try: 
                        [t,v] = m[0].strip(";").split(" ")
                        t0 = t
                        t = typeLookup.get(t,t)
                    except:
                        t,v = '', m[0]
                        t0 = t
                    v0 = v
                    v = "{}'{}".format(name, v)
                    print("{} {} :: {} -- ^ {}".format(pre, v, t, m[1]))
                    dat.append([v, v0, t, t0, m[1]])
                print("    } deriving (Show, Eq)")
            print()
            print("instance Storable {} where".format(name))
            print("    sizeOf _  = {{#sizeof  {}#}}".format(name))
            print("    alignOf _ = {{#alignof {}#}}".format(name))
            print("    peek p = {}".format(name))
            first = True
            for [v, v0, t, t0, d] in dat:
                # <$> liftM (toEnum.fromEnum) ({#get FT_DEVICE_LIST_INFO_NODE->Flags #} p)
                pre = "<$>" if first else "<|>"; first = False
                cast = castLookup.get(t, "(to"+t+")")
                print("           {} liftM {} ({{#get {}->{} #}} p) -- {}".format(pre, cast, name, v0, t0))
            print("    poke p x = do")
            for [v, v0, t, t0, d] in dat:
                cast = castLookup.get(t, "(to"+t+")")
                print("        {{#set {}.{} #}} p ({} $ {} x) -- {}".format(
                    name, v0, cast, mkHsName(v), t0
                ))
        elif (('' if description is None else description)+startText).lower().find('this api') >= 0:
            typ = "FUN"
        elif (('' if description is None else description)+startText).lower().find('the api') >= 0:
            typ = "FUN"
        elif (('' if description is None else description)+startText).lower().find('function pointer') >= 0:
            typ = "FUNPTR"
        else:
            typ = "DEFINE"
            print("{{#const {}#}}".format(name))
        if typ == 'FUN':
            params = info.get('Parameters', None)
            returns = info.get('Returns', None)
            print("{{#fun unsafe {} as {}".format(name, "_"+unTitle(name)))
            exportList.append(unTitle(name))
            if params is not None:
                first = True
                for p in params:
                    pre = "  ," if not first else "  {"
                    first = False
                    l=re.split(' +',p[0])
                    varName = l.pop()
                    varType = "".join(l)
                    print("{} alloca- `{}' peek* -- ^ {}: {}".format(pre, mkHsType(varType), varName, p[1]))
            print("  }")
            if returns is not None:
                print(indent(commentAbove("Returns\n"+ "\n".join(returns).replace('•','*')),2))
            print("  -> `CyReturnStatus' toCyReturnStatus")
            print("#}")

        print()

print()
print("module USBSerial (")
for i in exportList:
    print("    "+i+",")
print(")")

#{#fun unsafe FT_GetBitMode as _ftGetBitmode
#  { id `FtHandle'                 -- ^ FT_HANDLE ftHandle Handle of the device.
#  , alloca- `Word8' peekWord8*    -- ^ PUCHAR  pucMode Pointer to unsigned char to store the instantaneous data bus value.
#  }
#  -> `FtStatus' toFtStatus
##}