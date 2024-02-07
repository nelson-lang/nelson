%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function struct_res = test_runfile(varargin)
  % internal function
  % not documented
  with_mpi = false;
  filename = varargin{1};
  if length(varargin) == 2
    with_mpi = logical(varargin{2});
  end
  if ~isfile(filename)
    error(_('An existing filename expected.'));
  end

  feature('currentAxesOnClick', false);
  feature('currentFigureOnClick', false);
  
  r = false;
  msg = '';
  timing = 0;
  test_run_options = test_parsetags(filename);
  
  if (test_run_options.check_ref)
    [p, f, e] = fileparts(filename);
    dia_ref = [p, '/', f, '.ref'];
    if ~isfile(dia_ref)
      error(_('reference diary expected.'));
    end
    
    [p, f, e] = fileparts(filename);
    dia_res = [tempdir(), '', f, '.res'];
    rm = rmfile(dia_res);
  end
  
  current_path_test_runfile = path();
  % 'test_run_options' variable protected of clear('all') in test files
  varlock('local', 'test_run_options')
  varlock('local', 'current_path_test_runfile')
  ticBeginRunFile = time('ns');
  varlock('local', 'ticBeginRunFile');
  output_test = evalc('r = run(filename, ''errcatch'');')';
  ticEndRunFile = time('ns');
  timing = (double(ticEndRunFile) - double(ticBeginRunFile)) * 1e-9;
  varunlock('local','ticBeginRunFile');
  varunlock('local','test_run_options');
  varunlock('local', 'current_path_test_runfile')
  path(current_path_test_runfile);
  
  if ~r
    msg = lasterror();
  else
    msg = '';
  end
  
  if (test_run_options.check_ref)
    fileID = fopen(dia_res, 'wt');
    output_test = replace(output_test(:)', char([13 10]), char(10));
    fwrite(fileID, output_test, 'char');
    fclose(fileID);
    res = diff_file(dia_ref, dia_res, false);
    if (strcmp(res, '') == false)
      r = false;
      msg = {_('output res and ref are not equal.'), dia_res, dia_ref, res};
    end
  end
  struct_res.r = r;
  struct_res.msg = msg;
  struct_res.timing = timing;
end
