%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--ENGLISH IMPOSED-->
%=============================================================================
% Retro-read contract (format freeze, 1.0.0-beta.1): every frozen golden
% .nflow committed under tests/golden_format must keep loading in all later
% versions. Each golden is loaded, saved, reloaded and saved again:
%   - the first save must preserve the golden content (no field lost),
%   - load -> save must be idempotent (second save identical to the first).
% The goldens are frozen files: never regenerate them; when the format
% evolves, new readers must keep reading these exact bytes (migrating if
% needed).
%=============================================================================
golden_dir = [modulepath('nflow_engine', 'tests'), filesep, 'golden_format'];
files = dir([golden_dir, filesep, '*.nflow']);
assert_isequal(numel(files), 5);
bdclose('all');
for i = 1:numel(files)
  src = [golden_dir, filesep, files(i).name];
  golden = jsondecode(fileread(src));
  h = load_system(src);
  name = get_param(h, 'Name');
  f1 = [tempname(), '.nflow'];
  save_system(name, f1);
  bdclose(name);
  h2 = load_system(f1);
  f2 = [tempname(), '.nflow'];
  save_system(get_param(h2, 'Name'), f2);
  bdclose('all');
  a = jsondecode(fileread(f1));
  b = jsondecode(fileread(f2));
  % Idempotent round-trip: save -> load -> save is stable.
  assert_istrue(isequal(a, b));
  % Nothing from the frozen golden is lost or altered by load -> save.
  assert_istrue(isequal(golden, a));
  rmfile(f1);
  rmfile(f2);
end
%=============================================================================
% A file written by a newer format generation is rejected with a clear error.
f = [tempname(), '.nflow'];
fid = fopen(f, 'wt');
fwrite(fid, '{"format":"nflow-99","name":"future","blocks":[],"connections":[]}');
fclose(fid);
msg = '';
try
  load_system(f);
catch e
  msg = e.message;
end
assert_istrue(~isempty(strfind(msg, 'nflow-99')));
assert_istrue(~isempty(strfind(msg, 'newer version')));
rmfile(f);
%=============================================================================
% The pre-2 legacy layout stays rejected.
f = [tempname(), '.nflow'];
fid = fopen(f, 'wt');
fwrite(fid, '{"format":"nflow-1","name":"legacy","blocks":[],"connections":[]}');
fclose(fid);
msg = '';
try
  load_system(f);
catch e
  msg = e.message;
end
assert_istrue(~isempty(strfind(msg, 'legacy format ''nflow-1''')));
rmfile(f);
%=============================================================================
% A missing "format" field keeps loading (files predating the identifier),
% and a saved document carries the current format identifier.
f = [tempname(), '.nflow'];
fid = fopen(f, 'wt');
fwrite(fid, '{"name":"noformat","blocks":[],"connections":[],"sampleTime":0.1,"runtime":1}');
fclose(fid);
load_system(f);
bdclose('all');
rmfile(f);
new_system('nf_retro_fmt');
f = [tempname(), '.nflow'];
save_system('nf_retro_fmt', f);
bdclose('all');
d = jsondecode(fileread(f));
assert_isequal(d.format, 'nflow-2');
rmfile(f);
%=============================================================================
