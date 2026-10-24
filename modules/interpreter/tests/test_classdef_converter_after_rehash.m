%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% A function called with an object is probed as a class conversion. A file
% found not to be a classdef is remembered until the function tables change:
% once rewritten as a classdef and rehashed, the name is a class again.
%=============================================================================
test_dir = [tempname(), '_converter_rehash'];
mkdir(test_dir);
addpath(test_dir);
%=============================================================================
fid = fopen([test_dir, '/ConvRehashSource.m'], 'wt');
fprintf(fid, 'classdef ConvRehashSource < handle\n');
fprintf(fid, 'end\n');
fclose(fid);
fid = fopen([test_dir, '/ConvRehashTarget.m'], 'wt');
fprintf(fid, 'function r = ConvRehashTarget(x)\n');
fprintf(fid, '  r = ''function'';\n');
fprintf(fid, 'end\n');
fclose(fid);
rehash();
%=============================================================================
s = ConvRehashSource();
for k = 1:3
  asserts.isequal(ConvRehashTarget(s), 'function');
end
%=============================================================================
fid = fopen([test_dir, '/ConvRehashTarget.m'], 'wt');
fprintf(fid, 'classdef ConvRehashTarget\n');
fprintf(fid, '  properties\n');
fprintf(fid, '    Payload = 1\n');
fprintf(fid, '  end\n');
fprintf(fid, '  methods\n');
fprintf(fid, '    function obj = ConvRehashTarget(x)\n');
fprintf(fid, '    end\n');
fprintf(fid, '  end\n');
fprintf(fid, 'end\n');
fclose(fid);
rehash();
asserts.isequal(properties('ConvRehashTarget'), {'Payload'});
asserts.isequal(class(ConvRehashTarget(s)), 'ConvRehashTarget');
%=============================================================================
rmpath(test_dir);
clear s
rmdir(test_dir, 's');
%=============================================================================
