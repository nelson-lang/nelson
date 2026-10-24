%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function source = nflow_dashboard_generate(d, stem, target)
  model = [tempdir(), stem, '.nflow'];
  fid = fopen(model, 'wt');
  fwrite(fid, jsonencode(d));
  fclose(fid);
  nflow_codegenerate(model, tempdir(), true, target);
  if strcmp(target, 'rust')
    source = fileread([tempdir(), stem, filesep, 'src', filesep, 'lib.rs']);
  else
    source = fileread([tempdir(), stem, filesep, 'model.c']);
  end
  nflow_dashboard_verify_generated_setter([tempdir(), stem], target);
end

function nflow_dashboard_verify_generated_setter(folder, target)
  if strcmp(target, 'rust')
    if ~__nflow_codegen_toolchain__('rust')
      return
    end
    testsFolder = [folder, filesep, 'tests'];
    if ~isfolder(testsFolder)
      mkdir(testsFolder);
    end
    testSource = strjoin({...
      'use nflow_model::{nflow_step, ModelInput, ModelOutput, ModelState};', ...
      '#[test]', ...
      'fn dashboard_setter_applies_on_the_next_step() {', ...
      '    let mut state = ModelState::default();', ...
      '    let input = ModelInput::default();', ...
      '    let mut output = ModelOutput::default();', ...
      '    nflow_step(&mut state, &input, &mut output, 0.0);', ...
      '    assert!((output.y - 6.0).abs() < 1e-12);', ...
      '    state.set_dashboard_control_ui(5.0);', ...
      '    nflow_step(&mut state, &input, &mut output, 0.1);', ...
      '    assert!((output.y - 10.0).abs() < 1e-12);', ...
      '}'}, newline());
    nflow_dashboard_write_text([testsFolder, filesep, 'dashboard_setter.rs'], testSource);
    [status, output] = system(['cargo test --quiet --manifest-path "', ...
      folder, filesep, 'Cargo.toml"'], 120);
    assert_isequal(status, 0, output);
    return
  end

  if ~__nflow_codegen_toolchain__('C')
    return
  end
  testSource = strjoin({...
    '#include <math.h>', ...
    '#include "model.c"', ...
    'int main(void) {', ...
    '  ModelState state;', ...
    '  ModelInput input = {0};', ...
    '  ModelOutput output = {0};', ...
    '  nflow_init(&state);', ...
    '  nflow_step(&state, &input, &output, 0.0);', ...
    '  if (fabs(output.y - 6.0) > 1e-12) return 1;', ...
    '  if (!nflow_set_dashboard_control_ui(&state, 5.0)) return 2;', ...
    '  nflow_step(&state, &input, &output, 0.1);', ...
    '  return fabs(output.y - 10.0) > 1e-12;', ...
    '}'}, newline());
  nflow_dashboard_write_text([folder, filesep, 'dashboard_setter.c'], testSource);
  fid = fopen([folder, filesep, 'CMakeLists.txt'], 'at');
  fwrite(fid, [newline(), 'add_executable(dashboard_setter dashboard_setter.c)', newline()]);
  fclose(fid);
  buildFolder = [folder, filesep, 'setter-build'];
  [status, output] = system(['cmake -S "', folder, '" -B "', buildFolder, '"'], 120);
  assert_isequal(status, 0, output);
  [status, output] = system(['cmake --build "', buildFolder, ...
    '" --config Release --target dashboard_setter'], 120);
  assert_isequal(status, 0, output);
  executable = [buildFolder, filesep, 'dashboard_setter'];
  if ispc()
    executable = [buildFolder, filesep, 'Release', filesep, 'dashboard_setter.exe'];
  end
  [status, output] = system(['"', executable, '"'], 30);
  assert_isequal(status, 0, output);
end

function nflow_dashboard_write_text(filename, content)
  fid = fopen(filename, 'wt');
  fwrite(fid, content);
  fclose(fid);
end
