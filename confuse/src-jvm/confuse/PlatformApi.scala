package confuse

trait PlatformApi:
  self: MainApi =>

  def watch(
    paths: Iterable[os.FilePath] = Seq(),
    pwd: os.Path = os.pwd,
    readers: Map[String, FileReader] = Map(),
    env: Map[String, String] = sys.env,
    envPrefix: String = null,
    envKeyReplacer: String => String = defaultEnvKeyReplacer,
    envBinds: Iterable[(String, String)] = Map(),
    props: collection.Map[String, String] = sys.props,
    propsPrefix: String = null,
    propsBinds: Iterable[(String, String)] = Map(),
    args: Iterable[(String, String)] = Map(),
    dest: Config = Config(),
    onPreUpdate: Set[os.Path] => Unit = _ => (),
    onUpdate: (Config, Config) => Unit,
    onError: ReadException => Unit
  ): java.io.Closeable =

    @volatile var oldCfg = dest
    def fn() =
      try
        val newCfg = read(paths, pwd, readers, env, envPrefix, envKeyReplacer, envBinds, props, propsPrefix, propsBinds, args, dest)
        onUpdate(oldCfg, newCfg)
        oldCfg = newCfg
      catch
        case ex: ReadException => onError(ex)

    fn()
    Watcher(
      paths.map(fp => os.Path(fp, pwd)),
      changed =>
        onPreUpdate(changed)
        fn()
    )
