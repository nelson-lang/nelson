%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function r = test_makeref(filename)
  if ~isfile(filename)
    error(_('An existing filename expected.'));
  end
  
  options = test_parsetags(filename);
  if (options.check_ref == false)
    error(_('<--CHECK REF--> tag expected.'));
  end
  [p, f, e] = fileparts(filename);
  if strcmp(p, '') == true;
    p = pwd();
    filename = [p, '/', f, e];
  end
  dia_ref = [p, '/', f, '.ref'];
  % delete previous .ref
  rm = rmfile(dia_ref);
  r = test_makerefile(filename, options, dia_ref);
  r = (r == 1);
end
%=============================================================================
function r = test_makerefile(filename, options, ref_dest)
  cmd = '';
  if (options.english_imposed)
    cmd = [' --language', ' ', 'en_US'];
  end
  
  evalc_cmd_part = ['output=evalc(''r=run(''''', filename, ''''',''''errcatch'''');'');'];
  save_cmd_part = ['filewrite(''', ref_dest, ''', output);exit(double(r));' ];
  cmd_to_execute = ['"', evalc_cmd_part, save_cmd_part, '"'];
  
  cmd = [cmd, ' --quiet', ' --execute', ' ', cmd_to_execute];
  
  nelson_bin_path = modulepath('nelson', 'bin');
  
  if options.cli_mode
    nelson_exe_path = ['"', nelson_bin_path, '/', 'nelson-cli', '"'];
  else
    if options.gui_mode
      nelson_exe_path = ['"', nelson_bin_path, '/', 'nelson-gui', '"'];
    else
      if options.adv_cli_mode
        nelson_exe_path = ['"', nelson_bin_path, '/', 'nelson-adv-cli', '"'];
      else
        nelson_exe_path = ['"', nelson_bin_path, '/', 'nelson-cli', '"'];
      end
    end
  end
  cmd = [nelson_exe_path, ' ' , cmd];
  [r, msg] = unix(cmd);
end
%=============================================================================
