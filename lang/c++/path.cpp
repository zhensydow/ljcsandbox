/* Local Variables: */
/* compile-command: "g++ -Wall -Wextra -Og -std=c++17 main.cpp" */
/* End: */
#include <cstdlib>
#include <optional>
#include <vector>
#include <map>
#include <random>
#include <chrono>
#include <unordered_set>
#include <unordered_map>
#include <string_view>

/*----------------------------------------------------------------------------*/
std::string foldername( const std::string & path )
{
	return { path, 0, path.rfind('/') };
}

/*----------------------------------------------------------------------------*/
std::vector<std::string> split_folders( const std::string & path ){
	std::vector<std::string> folders;

	auto it = path.find( '/' );
	auto jt = 0;
	while( it != std::string::npos ){
 		const auto folder = std::string( path, jt, it - jt );
		jt = it + 1;
		it = path.find( '/', jt );
		if( folder.size() > 0 ){
			folders.push_back( folder );
		}
	}

	const auto last_folder = std::string( path, jt );
	if( last_folder.size() > 0 ){
		folders.push_back( last_folder );
	}

	return folders;
}

/*----------------------------------------------------------------------------*/
const std::vector<std::string> c_folders = {
	"/animations", "/characters", "/engine",
	"/example",	"/external", "/forest",
	"/game1", "/game2",	"/game3",
	"/house", "/internal", "/models",
	"/private",	"/public", "/rootpath",
	"/sounds", "/textures", "/xxxx",
	"/yyyy", "/zzzz",
};

const std::vector<std::string> c_filenames = {
	"/tree01", "/tree02", "/player",
	"/house01",	"/house02", "/rock01",
	"/rock02", "/cloud",	"/space",
	"/star01", "/star02", "/box01",
	"/box02", "/car01", "/car02"
};

const std::vector<std::string> c_extensions = {
	".txt", ".dat", ".png",
	".wav",	".dll", ".bmp",
	".raw",
};

std::vector<std::pair<std::string,int>> generate_modules( int count ){
	constexpr double SUBFOLDER_P = 0.2;
	std::vector<std::pair<std::string,int>> modules;
	std::unordered_set<std::string> uniques;

	modules.reserve( count );

    std::random_device rd;
    std::mt19937 gen(rd());
    std::uniform_int_distribution<> dis1( 2, 6 );
    std::uniform_int_distribution<> dis2( 0, c_folders.size() - 1 );
	std::uniform_real_distribution<> dis3( 0.0, 1.0 );

	int i = 0;
	while( i < count ){
		std::string folder;
		if( modules.size() > 5 and dis3(gen) < SUBFOLDER_P ){
			std::uniform_int_distribution<> dis4( 0, modules.size() - 1 );
			folder = modules[dis4(gen)].first.data();
			folder = folder + c_folders[dis2(gen)];
		}else{
			const auto parents = dis1(gen);
			for( int j = 0 ; j < parents ; ++j ){
				folder = folder + c_folders[dis2(gen)];
			}
		}

		if( uniques.count( folder ) == 0){
			modules.push_back( {folder, i} );
			uniques.insert( folder );
			++i;
		}
	}

	return modules;
}

std::vector<std::string> generate_assets(
	std::vector<std::pair<std::string,int>> modules, int count )
{
	std::vector<std::string> assets;
	std::unordered_set<std::string> uniques;

	assets.reserve( count );

    std::random_device rd;
    std::mt19937 gen(rd());
    std::uniform_int_distribution<> dis1( 0, 2 );
    std::uniform_int_distribution<> dis2( 0, c_folders.size() - 1 );
    std::uniform_int_distribution<> dis3( 0, modules.size() - 1 );
    std::uniform_int_distribution<> dis4( 0, c_filenames.size() - 1 );
    std::uniform_int_distribution<> dis5( 0, c_extensions.size() - 1 );

	int i = 0;
	while( i < count ){
		std::string asset = modules[dis3(gen)].first.data();
		const auto parents = dis1(gen);
		for( int j = 0 ; j < parents ; ++j ){
			asset = asset + c_folders[dis2(gen)];
		}

		asset += c_filenames[dis4(gen)];
		asset += c_extensions[dis5(gen)];

		if( uniques.count(asset) == 0){
			uniques.insert( asset );
			assets.push_back( std::move(asset) );
			++i;
		}
	}

	return assets;
}

/*----------------------------------------------------------------------------*/
template<typename TM, typename TC>
void fill( const TM & modules, TC & collection ){
	for( const auto & item: modules ){
		collection.insert( item.first, item.second );
	}
}

template<typename TA, typename TC>
void test_check( const TA & assets, const TC & collection ){
	for( const auto & path: assets ){
		const auto val = collection.find( path );
		if (val){
			printf("founded %s = %d\n", path.data(), val.value());
		} else {
			printf("not founded %s\n", path.data());
		}
	}
}

template<typename TA, typename TC>
size_t test_profile( const TA & assets, const TC & collection ){
	size_t founded = 0;
	for( const auto & path: assets ){
		const auto val = collection.find( path );
		if (val){
			++founded;
		}
	}

	return founded;
}

/*----------------------------------------------------------------------------*/
class SimpleDataModules{
public:
	void insert( const std::string & path, int val );
	std::optional<int> find( const std::string & path ) const;
	size_t count_modules() const { return m_modules.size(); }

private:
	std::map<std::string, int> m_modules;
};

void SimpleDataModules::insert( const std::string & path, int val ){
	m_modules[ path ] = val;
}

std::optional<int> SimpleDataModules::find( const std::string & path ) const{
	std::optional<int> value;
	size_t maxSize = 0;

	for( const auto & item: m_modules ){
		const auto pos = path.find( item.first );
		if( pos != std::string::npos and path[pos] == '/' ){
			if( item.first.size() > maxSize ){
				value = item.second;
				maxSize = item.first.size();
			}
		}
	}

	return value;
}

/*----------------------------------------------------------------------------*/
class HashDataModules{
public:
	void insert( const std::string & path, int val );
	std::optional<int> find( const std::string & path ) const;
	size_t count_modules() const { return m_modules.size(); }

private:
	std::unordered_map<size_t, int> m_modules;

};

void HashDataModules::insert( const std::string & path, int val ){
	m_modules[std::hash<std::string_view>{}(std::string_view(path))] = val;
}

std::optional<int> HashDataModules::find( const std::string & path ) const{
	auto it = path.rfind( '/' );
	while( it != std::string::npos and it != 0 ){
		const auto subfolder = std::string_view( path.data(), it );
		const auto h = std::hash<std::string_view>{}( subfolder );
		const auto search = m_modules.find( h );

		if( search != m_modules.end() ){
		 	return search->second;
		}

		it = path.rfind( '/', it - 1 );
	}

	return {};
}

/*----------------------------------------------------------------------------*/
void check(){
	// generate data
	const auto modules = generate_modules( 10 );
	for( const auto & module: modules ){
		printf( "mod: '%s' %d\n", module.first.data(), module.second );
	}

	printf( "\n" );

	const auto assets = generate_assets( modules, 10 );
	for( const auto & asset: assets ){
		printf( "ass: '%s'\n", asset.data() );
	}

	// test 1
	printf("\nSimpleDataModules:\n");

	SimpleDataModules coll1;

	fill( modules, coll1 );

	test_check( assets, coll1 );

	// test 2
	printf("\nHashDataModules:\n");

	HashDataModules coll2;

	fill( modules, coll2 );

	test_check( assets, coll2 );
}

/*----------------------------------------------------------------------------*/
template<typename T>
void profile(){
	constexpr int COUNT_TESTS = 5;
	constexpr auto COUNT_ASSETS = 10000;

	const std::vector<int> modules_sizes = {10, 100, 250, 500, 750, 1000, 10000};

	printf( "count modules;init collection;find 1K assets\n" );
	for( const auto count_modules : modules_sizes ){
		// generate data
		const auto modules = generate_modules( count_modules );
		const auto assets = generate_assets( modules, COUNT_ASSETS );

		std::array<double, 2> sums = {0, 0};

		for( int i = 0 ; i < COUNT_TESTS ; ++i ){
			T collection;

			auto t0 = std::chrono::high_resolution_clock::now();
			fill( modules, collection );
			auto t1 = std::chrono::high_resolution_clock::now();
			auto founded = test_profile( assets, collection );
			auto t2 = std::chrono::high_resolution_clock::now();
			std::chrono::duration<double> diff1 = t1 - t0;
			std::chrono::duration<double> diff2 = t2 - t1;

			if( founded != assets.size()){
				printf( "founded %ld of %ld (%ld modules)\n",
					 founded, assets.size(), collection.count_modules() );
			}

			sums[0] += diff1.count();
			sums[1] += diff2.count();
		}

		sums[0] = sums[0] / double(COUNT_TESTS);
		sums[1] = sums[1] / double(COUNT_TESTS);

		printf( "%d;%0.8f;%0.8f\n",
				count_modules, sums[0],
				sums[1]*(1000.0/float(COUNT_ASSETS)) );
	}
}

/*----------------------------------------------------------------------------*/
int main()
{
	printf( "Check it is working\n" );
 	check();

	printf( "\nMeasure performance\n" );
	printf( "\n Simple Find\n" );
	profile<SimpleDataModules>();
	printf( "\n Hash Find\n" );
	profile<HashDataModules>();

	return EXIT_SUCCESS;
}

/*----------------------------------------------------------------------------*/
