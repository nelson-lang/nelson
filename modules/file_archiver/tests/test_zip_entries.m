%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Archive names and entry names follow the reference rules.
%=============================================================================
WORK = [tempdir(), createGUID()];
DATA = [WORK, '/data'];
OUT = [WORK, '/out'];
mkdir(DATA);
mkdir([DATA, '/sub']);
mkdir(OUT);
filewrite([DATA, '/Sub.txt'], 'hi');
filewrite([DATA, '/a+b1.txt'], 'hello');
filewrite([DATA, '/sub/x.txt'], 'x');
%=============================================================================
% '.zip' is added only when the name has no extension (Nelson keeps .nlx, .fmu, ...).
cd(DATA);
zip([OUT, '/noext'], 'Sub.txt');
asserts.istrue(isfile([OUT, '/noext.zip']));
asserts.isfalse(isfile([OUT, '/noext']));
mkdir([OUT, '/dir.d']);
zip([OUT, '/dir.d/noext'], 'Sub.txt');
asserts.istrue(isfile([OUT, '/dir.d/noext.zip']));
zip([OUT, '/other.bar'], 'Sub.txt');
asserts.istrue(isfile([OUT, '/other.bar']));
asserts.isfalse(isfile([OUT, '/other.bar.zip']));
asserts.isequal(unzip([OUT, '/other.bar'], [OUT, '/bar']), {[OUT, '/bar/Sub.txt']});
zip([OUT, '/upper.ZIP'], 'Sub.txt');
asserts.istrue(isfile([OUT, '/upper.ZIP']));
asserts.isfalse(isfile([OUT, '/upper.ZIP.zip']));
%=============================================================================
% Wildcards accept characters that are special in regular expressions.
R = zip([OUT, '/w.zip'], 'a+b*.txt');
asserts.isequal(R, {'a+b1.txt'});
R = zip([OUT, '/w2.zip'], 'A+B?.TXT');
asserts.isequal(R, {'a+b1.txt'});
%=============================================================================
% Relative names are stored relative to the root folder, with or without '/'.
cd(tempdir());
R = zip([OUT, '/r1.zip'], 'Sub.txt', DATA);
asserts.isequal(R, {'Sub.txt'});
R = zip([OUT, '/r2.zip'], 'Sub.txt', [DATA, '/']);
asserts.isequal(R, {'Sub.txt'});
R = zip([OUT, '/r3.zip'], 'sub/x.txt', DATA);
asserts.isequal(R, {'sub/x.txt'});
%=============================================================================
% Absolute names are stored relative to their parent folder.
R = zip([OUT, '/a1.zip'], [DATA, '/Sub.txt']);
asserts.isequal(R, {'Sub.txt'});
R = zip([OUT, '/a2.zip'], [DATA, '/sub/x.txt']);
asserts.isequal(R, {'x.txt'});
R = zip([OUT, '/a3.zip'], [DATA, '/sub/x.txt'], DATA);
asserts.isequal(R, {'x.txt'});
R = zip([OUT, '/a4.zip'], [DATA, '/*.txt']);
asserts.isequal(R, {'Sub.txt', 'a+b1.txt'});
R = zip([OUT, '/a5.zip'], DATA);
asserts.isequal(R, {'data/Sub.txt', 'data/a+b1.txt', 'data/sub/x.txt'});
R = zip([OUT, '/a6.zip'], [DATA, '/']);
asserts.isequal(R, {'Sub.txt', 'a+b1.txt', 'sub/x.txt'});
%=============================================================================
% Round trip of an archive made with a root folder.
R = unzip([OUT, '/r1.zip'], [OUT, '/r1']);
asserts.isequal(R, {[OUT, '/r1/Sub.txt']});
asserts.isequal(fileread([OUT, '/r1/Sub.txt']), 'hi');
%=============================================================================
% Only empty folders get an entry, listed without '/'; wildcards expand matched folders.
TREE = [WORK, '/tree'];
mkdir([TREE, '/empty']);
mkdir([TREE, '/full/inner_empty']);
mkdir([TREE, '/full/sub']);
filewrite([TREE, '/a.txt'], 'a');
filewrite([TREE, '/full/y.txt'], 'y');
filewrite([TREE, '/full/sub/x.txt'], 'x');
cd(TREE);
asserts.isequal(zip([OUT, '/e1.zip'], 'empty'), {'empty'});
asserts.isequal(zip([OUT, '/e2.zip'], {'a.txt', 'empty'}), {'a.txt', 'empty'});
asserts.isequal(zip([OUT, '/e3.zip'], 'full'), {'full/inner_empty', 'full/sub/x.txt', 'full/y.txt'});
asserts.isequal(zip([OUT, '/e4.zip'], [TREE, '/full/']), {'inner_empty', 'sub/x.txt', 'y.txt'});
asserts.isequal(zip([OUT, '/e5.zip'], 'empty', TREE), {'empty'});
R = zip([OUT, '/e6.zip'], '*');
asserts.isequal(R, {'a.txt', 'empty', 'full/inner_empty', 'full/sub/x.txt', 'full/y.txt'});
R = unzip([OUT, '/e6.zip'], [OUT, '/e6']);
asserts.istrue(isfolder([OUT, '/e6/empty']));
asserts.istrue(isfolder([OUT, '/e6/full/inner_empty']));
asserts.istrue(isfile([OUT, '/e6/full/sub/x.txt']));
cd(WORK);
%=============================================================================
% Entries keep the file modification time (2 s ZIP precision), like the reference.
SOURCE = [nelsonroot(), '/modules/file_archiver/tests/test_zip.zip'];
zip([OUT, '/date.zip'], SOURCE);
R = unzip([OUT, '/date.zip'], [OUT, '/date']);
original = dir(SOURCE);
extracted = dir(R{1});
asserts.istrue(abs(extracted.datenum - original.datenum) * 86400 <= 2);
%=============================================================================
cd(tempdir());
rmdir(WORK, 's');
%=============================================================================
