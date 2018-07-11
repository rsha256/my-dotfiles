(* force autoload *)
Charting`PlotParser;

Unprotect[Charting`PlotParser];

DownValues[Charting`PlotParser] = DownValues[Charting`PlotParser] /. {VectorDensityPlot -> VectorDensityPlot | VectorPlot3D};

Protect[Charting`PlotParser];
