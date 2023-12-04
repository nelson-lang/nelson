%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
tic();

if ispc()
  XGETTEXT = ['"', nelsonroot(), '/tools/gettext/bin/xgettext.exe', '"'];
  MSGMERGE = ['"', nelsonroot(), '/tools/gettext/bin/msgmerge.exe', '"'];
  MSGCAT = ['"', nelsonroot(), '/tools/gettext/bin/msgcat.exe', '"'];
else
  XGETTEXT = 'xgettext';
  MSGMERGE = 'msgmerge';
  MSGCAT = 'msgcat';
end

MSGCAT_OPTIONS = '--lang=en_US --force-po --no-location';

XGETTEXT_KEYWORDS = '-k --keyword=dgettext --keyword=_ --keyword=_W --keyword=TR';
XGETTEXT_OPTION_NO_HEADER = '--omit-header';

XGETTEXT_OPTIONS = [XGETTEXT_OPTION_NO_HEADER, ' ', XGETTEXT_KEYWORDS, ' ', '-force-po', ' ', '--from-code=UTF-8'];

EXTENSIONS_SRC_NELSON = {'*.c' '*.h' '*.cpp' '*.cxx' '*.hxx' '*.hpp'};
EXTENSIONS_MACROS_NELSON = {'*.m'};
EXTENSIONS = [EXTENSIONS_SRC_NELSON, EXTENSIONS_MACROS_NELSON];
DOMAIN ='nelson';
TARGETDIR = [tempdir(), '', DOMAIN, '_pot'];
if isdir(TARGETDIR)
  rmdir(TARGETDIR, 's');
end
[status, msg] = mkdir(TARGETDIR);

FILESINFO_SRC = [];
for k = EXTENSIONS_SRC_NELSON
  f = dir([nelsonroot(),'/', k{1}], '-s');
  FILESINFO_SRC = [FILESINFO_SRC; f];
end

SRCLISTFILE = [TARGETDIR, '/', DOMAIN, '_src_lst_pot'];
fw = fopen(SRCLISTFILE, 'wt');
for k = 1:length(FILESINFO_SRC)
  srcfilename = sprintf('%s/%s', FILESINFO_SRC(k).folder, FILESINFO_SRC(k).name);
  fwrite(fw, [srcfilename, char(10)])
end
fclose(fw);

FILESINFO_MACROS = [];
for k = EXTENSIONS_MACROS_NELSON
  f = dir([nelsonroot(),'/', k{1}], '-s');
  FILESINFO_MACROS = [FILESINFO_MACROS; f];
end

MACROSLISTFILE = [TARGETDIR, '/', DOMAIN, '_macros_lst_pot'];
fw = fopen(MACROSLISTFILE, 'wt');
for k = 1:length(FILESINFO_MACROS)
  srcfilename = sprintf('%s/%s', FILESINFO_MACROS(k).folder, FILESINFO_MACROS(k).name);
  fwrite(fw, [srcfilename, char(10)])
end
fclose(fw);


SRC_POT = [TARGETDIR, '/', DOMAIN, '_src.pot'];
MACROS_POT = [TARGETDIR, '/', DOMAIN, '_macros.pot'];
ALL_POT = [TARGETDIR, '/', DOMAIN, '.pot'];

header_pot = poheader(DOMAIN, 'en_US');

src_cmd = [XGETTEXT, ' ', XGETTEXT_OPTIONS, ' -d ', DOMAIN, ' -o ', SRC_POT, ' -f ', SRCLISTFILE];
macros_cmd = [XGETTEXT, ' ', XGETTEXT_OPTIONS, ' --language=Python',  ' -d ', DOMAIN, ' -o ', MACROS_POT, ' -f ', MACROSLISTFILE];

[r, errmsg] = unix(src_cmd);
if r != 0
  error(errmsg);
end

[r, errmsg] = unix(macros_cmd);
if r != 0
  error(errmsg);
end

txt = fileread(SRC_POT, 'string');
txt = [string(header_pot); txt];
filewrite(SRC_POT, txt);

txt = fileread(MACROS_POT, 'string');
txt = [string(header_pot); txt];
filewrite(MACROS_POT, txt);

cat_cmd = [MSGCAT, ' ', MSGCAT_OPTIONS, ' ', SRC_POT, ' ', MACROS_POT, ' -o ', ALL_POT];
[r, errmsg] = unix(cat_cmd);
if r != 0
  error(errmsg);
end

potfile = [nelsonroot(), '/locale/nelson.pot'];
copyfile(ALL_POT, potfile, 'f');

langs = getavailablelanguages();

for l = langs(:)'
  disp([_('update .po for:'), l{1}])
  dirdest = [nelsonroot(), '/locale/', l{1}, '/LC_MESSAGES/'];
  if ~isdir(dirdest)
    mkdir(dirdest)
  end
  pofiledest = [dirdest, DOMAIN, '.po'];
  
  if isfile(pofiledest)
    MSGCAT_OPTIONS = ' --force-po --no-location --unique --use-first';
    CMD = [MSGCAT, ' ', MSGCAT_OPTIONS, ' ', pofiledest, ' ', potfile, ' -o ', pofiledest];
  else
    header_po = poheader(DOMAIN, l{1});
    filewrite(pofiledest, header_pot)
    CMD = [MSGMERGE, ' --update ', pofiledest, ' ', potfile];
  end
  [r, errmsg] = unix(CMD);
  if r != 0
    error(errmsg);
  end
end

toc()
exit('force')
%=============================================================================
