/*******************************************************************************
  compile with:

  g++ -Wall -O2 -o sdl_example sdl_example.cpp -lSDL

 */
//------------------------------------------------------------------------------
#include "SDL/SDL.h"
#include <cassert>
#include <list>
#include <cmath>
#include <cstdio>

//------------------------------------------------------------------------------
const int SCR_WIDTH = 640;
const int SCR_HEIGHT = 480;
const double DT = 1.0/30.0;
const double SCR_FACTOR = double(SCR_WIDTH) / double(SCR_HEIGHT);

SDL_Surface *g_screen = 0;

//------------------------------------------------------------------------------
void put_pixel32( SDL_Surface *surface, int x, int y, Uint32 pixel ){
  // if the surface must be locked
  if( SDL_MUSTLOCK( surface ) ){
    //Lock the surface
    SDL_LockSurface( surface );
  }
  // Convert the pixels to 32 bit
  Uint32 *pixels = (Uint32 *)surface->pixels;

  //Set the pixel
  pixels[ ( y * surface->w ) + x ] = pixel;

  if( SDL_MUSTLOCK( surface ) ){
    SDL_UnlockSurface( surface );
  }
}

//------------------------------------------------------------------------------
class Sprite{
public:
  Sprite( double x, double y, double vel ) :
    m_posx(x), m_posy(y), m_velocity( vel ){
  }

  void draw() const;
  void update();

private:
  double m_posx;
  double m_posy;
  double m_velocity;
};

//------------------------------------------------------------------------------
void
Sprite::update(){
  m_posx = m_posx + m_velocity * DT;
  if( m_posx > SCR_FACTOR ){
    m_posx -= SCR_FACTOR;
  }
}

//------------------------------------------------------------------------------
void
Sprite::draw() const{

  put_pixel32( g_screen,
	       round( m_posx * SCR_HEIGHT),
	       round( m_posy * SCR_HEIGHT ), 0xFFFFFFFF );
}

//------------------------------------------------------------------------------
int main( int argc, char* args[] ){
  SDL_Init( SDL_INIT_EVERYTHING );

  g_screen = SDL_SetVideoMode( SCR_WIDTH, SCR_HEIGHT, 32, SDL_SWSURFACE );
  assert( g_screen && "can't initialize video" );

  std::list<Sprite*> sprites;
  std::list<Sprite*>::iterator is;

  Sprite * spr = 0;

  for( int i = 1 ; i <= 20 ; ++i ){
    double vel = SCR_FACTOR / (2000.0/i);
    spr = new Sprite( 0.0, (10 + 20.0*i) / SCR_HEIGHT, vel );
    sprites.insert( sprites.end(), spr );
  }

  bool quit = false;
  SDL_Event event;
  Uint32 currentTime = SDL_GetTicks();
  double accum = 0;
  while( ! quit ){
    // get user input
    while( SDL_PollEvent( &event ) ){
      if( event.type == SDL_QUIT ){
	//Quit the program
	quit = true;
      }
    }

    // update world
    Uint32 newTime = SDL_GetTicks();
    double frameTime = (currentTime - newTime) / 1000.0;
    if( frameTime > 0.25 ){
      frameTime = 0.25;
    }
    currentTime = newTime;

    accum = accum + frameTime;
    while( accum > DT ){
      for( is = sprites.begin() ; is != sprites.end() ; ++is ){
	(*is)->update();
      }
      accum -= DT;
    }

    // draw world
    SDL_FillRect( g_screen, 0, 0x000000 );
    for( is = sprites.begin() ; is != sprites.end() ; ++is ){
      (*is)->draw();
    }

    SDL_Flip( g_screen );
  }

  for( is = sprites.begin() ; is != sprites.end() ; ++is ){
    delete (*is);
  }

  SDL_Quit();
  return 0;
}

//------------------------------------------------------------------------------
