namespace FSharp.Stats

open System.IO
open System.Collections.Generic

module ServiceLocator = 

    type Architecture =
        | X64 
        | X86

    type OS =
        | Windows 
        | Linux
        | Mac

    /// Generic provider with unmanaged DLL dependencies.
    type Provider<'a> = {
        Name          : string
        RequiredDLLs  : string seq
        Architecture  : Architecture  
        OS            : OS
        InitService   : unit -> 'a
    }

    let createProviderX64<'a> name requiredDLLs os initService : Provider<'a> =
        {Name=name;RequiredDLLs=requiredDLLs;Architecture=Architecture.X64;OS=os;InitService=initService}

    let createProviderX86<'a> name requiredDLLs os initService : Provider<'a>  =
        {Name=name;RequiredDLLs=requiredDLLs;Architecture=Architecture.X86;OS=os;InitService=initService}

    let setEnvironmentPathVariable dllDirectory =
        System.Environment.SetEnvironmentVariable("PATH", System.Environment.GetEnvironmentVariable("PATH") + ";" + dllDirectory)

    /// The DLLs search procedure for those DLLs
    //
    // The default behavior now is to look in all the system locations first, then the current directory, and finally any user-defined paths.
    // This will have an impact on your code if you install a DLL in the application's directory because Windows Server 2003 no longer loads
    // the 'local' DLL if a DLL of the same name is in the system directory. A common example is if an application won't run with a specific 
    // version of a DLL, an older version is installed that does work in the application directory. This scenario will fail in Windows Server 2003. 
    //
    // Search order:
    //   The Windows system directory. The GetSystemDirectory function retrieves the path of this directory.
    //   The Windows directory. The GetWindowsDirectory function retrieves the path of this directory.
    //   The directory where the executable module for the current process is located.
    //   The current directory.
    //   The directories listed in the PATH environment variable.
    let initSearchPaths () =
        let noLaterRepeats xs =
          let collect (soFar,revXs) x =
            if Set.contains x soFar then (soFar,revXs) else (Set.add x soFar,x::revXs)
          let (_,revXs) = List.fold collect (Set.empty,[]) xs
          revXs
          //List.rev revXs

        let windowsSystemDir = System.Environment.SystemDirectory
        let windowsDir       =
            windowsSystemDir
            |> System.IO.Path.GetDirectoryName
        let currentExeDirs   =
            // This includes EXE directory, and loaded DLL directories.
            // It may be an over-estimate of the search path.
            let proc = System.Diagnostics.Process.GetCurrentProcess()
            [ for m in proc.Modules do
                yield Path.GetDirectoryName m.FileName ]        
            |> List.distinct
        let currentDir = System.Environment.CurrentDirectory 
        let executingAssembly = System.Reflection.Assembly.GetExecutingAssembly().Location
        let pathDirs =
            match System.Environment.GetEnvironmentVariable("PATH") with
            | null  -> []
            | paths -> paths.Split([|';'|]) |> List.ofArray   
        
        windowsSystemDir :: windowsDir :: (currentExeDirs @ [currentDir;executingAssembly] @ pathDirs)
        //noLaterRepeats orderedSearchPaths

    // normalizes filename
    let normDllFilename (dll:string) = 
        (Path.GetFileName dll).ToLower() 

    let initDllPathTable () : Map<string,string list> =
        initSearchPaths ()
        |> Seq.collect (fun path ->  
            if not (Directory.Exists path) then
                seq [] 
            else
                Directory.GetFiles(path,"*.DLL")
                |> Seq.map (fun file -> normDllFilename file,path)
            )
        |> Seq.fold (fun state (file,path) -> 
            if state.ContainsKey file then 
                let tmp = state.[file]
                // rev path order !
                state |> Map.add file (path::tmp)
            else
                state |>  Map.add file ([path])
            ) Map.empty


    let isLoadableProvider (dllPaths:Map<string,string list>) (provider:Provider<'a>) =
        let isAvailable    (dll:string)  = dllPaths.ContainsKey(normDllFilename dll)
        provider.RequiredDLLs |> Seq.forall isAvailable


    let tryCheckProvider (dllPaths:Map<string,string list>) (provider:Provider<'a>) = 
        //let isAvailable    (dll:string)  = dllPaths.ContainsKey(normDllFilename dll)
        let availableReason (dll:string) = 
            dllPaths.[normDllFilename dll]
            |> Seq.map (fun path -> sprintf "Required %s seen in %s" dll path ) 
            |> String.concat "\n"
        if isLoadableProvider dllPaths provider then
            let justification = 
                provider.RequiredDLLs
                |> Seq.map availableReason
                |> String.concat "\n"
            Some (provider,("Provider: " + provider.Name + "\n" + justification))
        else
            None // "Provider is not loadable"

    
    

    type 'a ServiceState =
    | ServiceDisabled                          // service disabled, do not look for it.
    | ServiceEnabledUninitialised              // service enabled, but no search made yet.
    | ServiceEnabledOK     of 'a * string      // service enabled, and justification string for diagnostics.
    | ServiceEnabledFailed                     // service enabled, but DLLs not found, or load failed. 


    type ServiceProvider<'a>(providers:Provider<'a> seq) =    
        let mutable providers = Seq.toArray providers                // possible providers configuration state
        let mutable state = ServiceEnabledUninitialised               // service state
        let dllPaths = initDllPathTable () 

        /// Service Providers
        member this.Providers with get()  = providers
                               and set(x) = providers <- x
    
        /// Disable the service.
        member this.Stop()       = state <- ServiceDisabled

        /// Use the LAPACK service from the given provider.
        /// If the supporting DLLs are not available, this may fail (now or later).
        member this.StartWith(p:Provider<'a>) =
          let justification = 
              match tryCheckProvider dllPaths p with
              | None                   -> "The provider DLLs did not appear to be present, the service may fail"
              | Some (p,justification) -> justification      
          state <- ServiceEnabledOK (p.InitService(),justification)

        /// Start the service with the first provider that looks loadable.     
        member this.Start() =          
          let candidates = Array.choose (tryCheckProvider dllPaths) providers
          if candidates.Length=0 then                                     // guard
            state <- ServiceEnabledFailed
            false
          else
            let provider,justification = candidates.[0]
            state <- ServiceEnabledOK (provider.InitService(),justification)  // index covered by guard above
            true

        member this.GetPathMap() = dllPaths

        member this.Service() = 
          match state with
          | ServiceEnabledUninitialised -> this.Start() |> ignore
          | _ -> ()
          match state with
            | ServiceDisabled                          
            | ServiceEnabledUninitialised  // (The above initialisation call must have failed)
            | ServiceEnabledFailed                     -> None
            | ServiceEnabledOK (service,_) -> Some service
    
        member this.Available() =         
          match state with
            | ServiceDisabled            
            | ServiceEnabledFailed        
            | ServiceEnabledUninitialised -> false
            | ServiceEnabledOK (_,_)      -> true
                        
        member this.Status() =         
          match state with
            | ServiceDisabled                          -> "Disabled"           
            | ServiceEnabledFailed                     -> "Failed to start"
            | ServiceEnabledUninitialised              -> "Will auto enable on demand"
            | ServiceEnabledOK (service,justification) -> "Enabled\n" ^ justification

  



