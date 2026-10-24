%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Create and extract a ZIP archive entirely in the temporary directory.
sourceFolder = [tempdir(), 'nelson-archive-source/'];
outputFolder = [tempdir(), 'nelson-archive-output/'];
archive = [tempdir(), 'nelson-example.zip'];
if ~isdir(sourceFolder), mkdir(sourceFolder); end
if ~isdir(outputFolder), mkdir(outputFolder); end
filewrite([sourceFolder, 'notes.txt'], 'A small archived document.');
zip(archive, [sourceFolder, '*.txt']);
files = unzip(archive, outputFolder);
disp(files);
