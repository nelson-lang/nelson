%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 32-->
% <--ENGLISH IMPOSED-->
%=============================================================================
% The flattener processes one subsystem per pass with a 1000-pass guard. It
% used to fail OPEN: past the guard it returned a partially flattened diagram,
% which then surfaced as a confusing 'unknown type: subsystem' error. It must
% now fail CLOSED with a clear non-convergence message.
%=============================================================================
one = '{"id":"sK","type":"subsystem","inputs":0,"outputs":0,"params":{"subsystem":{"blocks":[],"connections":[]}}}';
parts = cell(1, 1001);
for k = 1:1001
  parts{k} = strrep(one, 'sK', ['s', int2str(k)]);
end
json = ['{"blocks":[', strjoin(parts, ','), '],"connections":[],"sampleTime":0.1}'];
model = [tempname(), '.nflow'];
fid = fopen(model, 'wt');
fwrite(fid, json);
fclose(fid);
%=============================================================================
msg = '';
try
  nflow_codegenerate(model, tempdir(), false, 'C');
catch e
  msg = e.message;
end
assert_istrue(~isempty(strfind(msg, 'did not converge')));
assert_istrue(isempty(strfind(msg, 'unknown')));
%=============================================================================
rmfile(model);
%=============================================================================
