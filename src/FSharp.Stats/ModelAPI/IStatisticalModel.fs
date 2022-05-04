namespace FSharp.Stats

type IStatisticalModel<'Design,'Response,'Domain,'Range> =
    /// Design contains the independent/explanatory variables used in the model
    abstract member Design: 'Design
    /// Observations used to train/derive the model (response or dependent variable)
    abstract member Response: 'Response
    /// model procedure name, e.g. "Univariable Ordinary Least Squares"
    abstract member ModelName: StatisticalModelName
    /// derive predictions from the model from model domain (not necessary equal to Design) to model range (not necessary equal to Response)
    abstract member Predict: 'Domain -> 'Range
    /// returns a formatted string containing the model summary
    abstract member FormatSummary: unit -> string
    /// prints a formatted string containing the model summary
    abstract member PrintSummary: unit -> unit
    /// formatted string containing the model summary for usage in .NET interactive
    abstract member InteractiveSummary: unit -> string
