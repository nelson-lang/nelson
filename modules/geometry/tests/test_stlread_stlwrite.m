%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.isequal(nargout('stlread'), 4);
asserts.isequal(nargout('stlwrite'), 0);
%=============================================================================
testDir = [tempname(), '/'];
if ~isfolder(testDir)
  mkdir(testDir);
end
cleanupTestDir = onCleanup(@() rmdir(testDir, 's')); %#ok<NASGU>
%=============================================================================
P = [0 0 0; 1 0 0; 0 1 0; 0 0 1];
T = [1 2 3; 1 3 4];
TR = triangulation(T, P);
binaryFile = [testDir, 'roundtrip_binary.stl'];
textFile = [testDir, 'roundtrip_text.stl'];
%=============================================================================
stlwrite(TR, binaryFile);
[TRb, fmtb, attrb, solidb] = stlread(binaryFile);
asserts.isequal(fmtb, 'binary');
asserts.isequal(TRb.Points, P);
asserts.isequal(TRb.ConnectivityList, T);
asserts.isequal(class(attrb), 'uint16');
asserts.isequal(size(attrb), [2 1]);
asserts.isequal(attrb, uint16([0; 0]));
asserts.isequal(solidb, [1; 1]);
%=============================================================================
stlwrite(TR, binaryFile, 'Attribute', uint16([7 9]));
[TRb, fmtb, attrb, solidb] = stlread(binaryFile);
asserts.isequal(fmtb, 'binary');
asserts.isequal(TRb.Points, P);
asserts.isequal(TRb.ConnectivityList, T);
asserts.isequal(attrb, uint16([7; 9]));
asserts.isequal(solidb, [1; 1]);
binaryCopyFile = [testDir, 'roundtrip_binary_copy.stl'];
stlwrite(TRb, binaryCopyFile, 'binary', 'Attribute', attrb);
[TRbc, fmtbc, attrbc, solidbc] = stlread(binaryCopyFile);
asserts.isequal(fmtbc, fmtb);
asserts.isequal(TRbc.Points, TRb.Points);
asserts.isequal(TRbc.ConnectivityList, TRb.ConnectivityList);
asserts.isequal(attrbc, attrb);
asserts.isequal(solidbc, solidb);
%=============================================================================
stlwrite(TR, textFile, 'text', 'SolidIndex', [3 5]);
[TRt, fmtt, attrt, solidt] = stlread(textFile);
asserts.isequal(fmtt, 'text');
asserts.isequal(TRt.Points, P);
asserts.isequal(TRt.ConnectivityList, T);
asserts.isequal(class(attrt), 'uint16');
asserts.isequal(size(attrt), [2 0]);
asserts.isequal(solidt, [1; 2]);
textCopyFile = [testDir, 'roundtrip_text_copy.stl'];
stlwrite(TRt, textCopyFile, 'text', 'SolidIndex', solidt);
[TRtc, fmttc, attrtc, solidtc] = stlread(textCopyFile);
asserts.isequal(fmttc, fmtt);
asserts.isequal(TRtc.Points, TRt.Points);
asserts.isequal(TRtc.ConnectivityList, TRt.ConnectivityList);
asserts.isequal(size(attrtc), size(attrt));
asserts.isequal(solidtc, solidt);
%=============================================================================
P2 = [0 0; 1 0; 0 1];
TR2 = triangulation([1 2 3], P2);
text2dFile = [testDir, 'two_dimensional_text.stl'];
stlwrite(TR2, text2dFile, 'text');
[TR2r, fmt2, attr2, solid2] = stlread(text2dFile);
asserts.isequal(fmt2, 'text');
asserts.isequal(TR2r.Points, [P2, zeros(3, 1)]);
asserts.isequal(TR2r.ConnectivityList, [1 2 3]);
asserts.isequal(size(attr2), [1 0]);
asserts.isequal(solid2, 1);
%=============================================================================
P4 = [0 0 0; 1 0 0; 0 1 0; 0 0 1; 1 1 1];
T4 = [1 2 3; 1 3 4; 2 3 5];
TR4 = triangulation(T4, P4);
interleavedFile = [testDir, 'solid_interleaved.stl'];
stlwrite(TR4, interleavedFile, 'text', 'SolidIndex', [2; 1; 2]);
[TR4r, ~, ~, solid4] = stlread(interleavedFile);
asserts.isequal(TR4r.ConnectivityList, [1 2 3; 1 4 2; 4 2 5]);
asserts.isequal(solid4, [1; 2; 2]);
%=============================================================================
textDegenerateFile = [testDir, 'degenerate_text.stl'];
fid = fopen(textDegenerateFile, 'wt');
fprintf(fid, 'solid part\n');
fprintf(fid, 'facet normal 0 0 1\nouter loop\n');
fprintf(fid, 'vertex 0 0 0\nvertex 1 0 0\nvertex 0 1 0\n');
fprintf(fid, 'endloop\nendfacet\n');
fprintf(fid, 'facet normal 0 0 0\nouter loop\n');
fprintf(fid, 'vertex 0 0 0\nvertex 0 0 0\nvertex 0 0 0\n');
fprintf(fid, 'endloop\nendfacet\nendsolid part\n');
fclose(fid);
[TRd, fmtd, attrd, solidd] = stlread(textDegenerateFile);
asserts.isequal(fmtd, 'text');
asserts.isequal(TRd.Points, [0 0 0; 1 0 0; 0 1 0]);
asserts.isequal(TRd.ConnectivityList, [1 2 3]);
asserts.isequal(size(attrd), [1 0]);
asserts.isequal(solidd, 1);
%=============================================================================
binaryDegenerateFile = [testDir, 'degenerate_binary.stl'];
fid = fopen(binaryDegenerateFile, 'wb');
fwrite(fid, uint8(zeros(1, 80)), 'uint8');
fwrite(fid, uint32(2), 'uint32');
fwrite(fid, single([0 0 1]), 'single');
fwrite(fid, single([0 0 0]), 'single');
fwrite(fid, single([1 0 0]), 'single');
fwrite(fid, single([0 1 0]), 'single');
fwrite(fid, uint16(42), 'uint16');
fwrite(fid, single([0 0 0]), 'single');
fwrite(fid, single([0 0 0]), 'single');
fwrite(fid, single([0 0 0]), 'single');
fwrite(fid, single([0 0 0]), 'single');
fwrite(fid, uint16(77), 'uint16');
fclose(fid);
[TRbd, fmtbd, attrbd, solidbd] = stlread(binaryDegenerateFile);
asserts.isequal(fmtbd, 'binary');
asserts.isequal(TRbd.Points, [0 0 0; 1 0 0; 0 1 0]);
asserts.isequal(TRbd.ConnectivityList, [1 2 3]);
asserts.isequal(attrbd, uint16(42));
asserts.isequal(solidbd, 1);
%=============================================================================
missingFile = [testDir, 'missing.stl'];
asserts.checkerror(['stlread(''', missingFile, ''')'], sprintf(_('Unable to open file: %s'), missingFile));
asserts.checkerror(['stlread(''', testDir, 'bad.obj'')'], _('File name must end with .stl or .STL.'));
% the file-name condition also carries a stable identifier
asserts.checkerror(['stlread(''', testDir, 'bad.obj'')'], _('File name must end with .stl or .STL.'), 'Nelson:stl:invalidFileName');
asserts.checkerror('stlwrite(eye(3), binaryFile)', _('Triangulation object expected.'));
asserts.checkerror('stlwrite(TR, [testDir, ''bad.obj''])', _('File name must end with .stl or .STL.'));
asserts.checkerror('stlwrite(TR, binaryFile, ''ascii'')', _('File format must be ''binary'' or ''text''.'));
asserts.checkerror('stlwrite(TR, textFile, ''text'', ''Attribute'', uint16([1; 2]))', _('''Attribute'' parameter is only supported when file format is ''binary''.'));
asserts.checkerror('stlwrite(TR, binaryFile, ''binary'', ''SolidIndex'', [1; 2])', _('''SolidIndex'' parameter is only supported when file format is ''text''.'));
asserts.checkerror('stlwrite(TR, binaryFile, ''Attribute'', uint16(1))', sprintf(_('%s must be a uint16 vector with one value for each triangle.'), 'Attribute'));
asserts.checkerror('stlwrite(TR, textFile, ''text'', ''SolidIndex'', 1)', sprintf(_('%s must be a numeric vector with one value for each triangle.'), 'SolidIndex'));
%=============================================================================
