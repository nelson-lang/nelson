%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--ADV-CLI MODE-->
%=============================================================================
% A handle to a local function of a file loaded with run() stays callable after
% run() returns: the file definition is owned by the run and freed when it ends,
% so the handle has to keep it alive. The handle finds that owner by walking up
% the prevFunction chain, which means the local functions must be chained onto
% the definition being run and not onto the one the parser built.
%=============================================================================
global HANDLE_FROM_RUN
global DELETE_FCN_RAN
global FIRST_GENERATION
global SECOND_GENERATION
HANDLE_FROM_RUN = [];
DELETE_FCN_RAN = false;
%=============================================================================
dir_test = [tempname(), '/'];
mkdir(dir_test);
file_test = [dir_test, 'local_handle_owner.m'];
%=============================================================================
fid = fopen(file_test, 'wt');
fprintf(fid, 'function local_handle_owner()\n');
fprintf(fid, '  global HANDLE_FROM_RUN\n');
fprintf(fid, '  HANDLE_FROM_RUN = @on_local;\n');
fprintf(fid, 'end\n');
fprintf(fid, 'function r = on_local(varargin)\n');
fprintf(fid, '  r = 42;\n');
fprintf(fid, 'end\n');
fclose(fid);
%=============================================================================
% the file is deliberately not on the search path: run() is what loads it
run(file_test);
asserts.isequal(class(HANDLE_FROM_RUN), 'function_handle');
asserts.isequal(func2str(HANDLE_FROM_RUN), 'on_local');
asserts.isequal(HANDLE_FROM_RUN(), 42);
%=============================================================================
% running the file again, and dropping the handle, must stay sound too
run(file_test);
asserts.isequal(HANDLE_FROM_RUN(), 42);
HANDLE_FROM_RUN = [];
%=============================================================================
% the same holds for a handle a graphics object holds and calls much later
file_deferred = [dir_test, 'local_handle_deferred.m'];
fid = fopen(file_deferred, 'wt');
fprintf(fid, 'function local_handle_deferred()\n');
fprintf(fid, '  f = figure(''Visible'', ''off'');\n');
fprintf(fid, '  set(f, ''DeleteFcn'', @on_delete_local);\n');
fprintf(fid, 'end\n');
fprintf(fid, 'function on_delete_local(src, event)\n');
fprintf(fid, '  global DELETE_FCN_RAN\n');
fprintf(fid, '  DELETE_FCN_RAN = true;\n');
fprintf(fid, 'end\n');
fclose(fid);
%=============================================================================
run(file_deferred);
close all;
drawnow();
asserts.istrue(DELETE_FCN_RAN);
%=============================================================================
%=============================================================================
% A handle built while an already-captured local runs - so after the run that
% owns the file has ended and been unregistered - has no owner left to pin. It
% has to stay callable anyway, on the retain the handle takes for itself.
file_second = [dir_test, 'local_handle_second_generation.m'];
fid = fopen(file_second, 'wt');
fprintf(fid, 'function local_handle_second_generation()\n');
fprintf(fid, '  global FIRST_GENERATION\n');
fprintf(fid, '  FIRST_GENERATION = @on_first;\n');
fprintf(fid, 'end\n');
fprintf(fid, 'function on_first()\n');
fprintf(fid, '  global SECOND_GENERATION\n');
fprintf(fid, '  SECOND_GENERATION = @on_second;\n');
fprintf(fid, 'end\n');
fprintf(fid, 'function r = on_second(varargin)\n');
fprintf(fid, '  r = 7;\n');
fprintf(fid, 'end\n');
fclose(fid);
%=============================================================================
run(file_second);
FIRST_GENERATION();
% dropping the first-generation handle drops the only pin on the run's chain
FIRST_GENERATION = [];
asserts.isequal(SECOND_GENERATION(), 7);
SECOND_GENERATION = [];
%=============================================================================
rmdir(dir_test, 's');
%=============================================================================
