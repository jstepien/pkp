from waflib.TaskGen import extension
from waflib import Task

APPNAME = 'libpkp'
VERSION = 'unstable'

@extension('.rs')
def hs_file(self, node):
  out_source = node.change_ext('')
  tsk = self.create_task('rustc')
  tsk.set_inputs(node)
  tsk.set_outputs(out_source)
  tsk.build_path = node.parent.get_bld().abspath()
  tsk.install_path = '${PREFIX}/lib'

class rustc(Task.Task):
  run_str = 'true && ${RUSTC} -O -o $(basename ${TGT}) ${SRC}'

def options(opt):
  opt.load('compiler_c')

def configure(conf):
  conf.load('compiler_c')
  conf.find_program('rustc', var='RUSTC')
  conf.find_program('xz')
  conf.find_program('unxz')
  conf.define('APPNAME', APPNAME)
  conf.define('VERSION', VERSION)
  conf.define('PREFIX', conf.options.prefix)
  conf.write_config_header('config.h')

def build(bld):
  bld.program(source='pkp.c libpkp.c', target='pkp', install_path=False)
  bld.shlib(source='libpkp.c', target='pkp')
  list(map(lambda file: bld.program(source=file + '.rs', target=file),
    ['decode', 'encode']))
  bld.install_files('${PREFIX}/include', 'pkp.h')
  bld.install_files('${PREFIX}/lib/' + APPNAME, 'encode decode', chmod=0o755)
