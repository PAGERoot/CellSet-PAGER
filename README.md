# CELLSET to PAGE-Root


`CELLSET to PAGE-Root` is a small shin app to transform output files from [CellSet](https://www.cpib.ac.uk/tools-resources/software/cellset/) (in XLSX format) into a single RSML file to could be read by [PAGE-Root](). 

## Launching the app

In the R terminal, run the following command:

	library(shiny)
	shiny::runGitHub("PAGERoot/CellSet-PAGER", "PAGERoot") 



## Converting files 

1. Enter the path to the folder contaning the different XLSX files outputed by CellSet. The folder should contain only these files

2. Clean up you data by matching the cell type in your datafiles to the allowed cell types. In the third pane, just select in each dropdown menu the corresponding cell type. Press `Update your data` to valide and make the change in your data file. Once you have as many cell types in you dataset as allowed, move to the next step

3. Downlaod your data in an [RSML](http://rootsystemml.github.io/) format



-----


> THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
