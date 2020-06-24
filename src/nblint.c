#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <math.h>
//#include <malloc.h>

#include "a.h"
//#include "fix.h"

static int initted = 0;

void gen_wsinc( void );

#define Fo (512)
#define Nz (13)
// must be odd
#define WSINCLEN (2*Nz*Fo+1)

#define HAMMING(t) (0.54 - 0.46 * cos( (t) ))

// plus 2 for padding
double wsinc_[WSINCLEN+2];
double *wsinc;
double dwsinc_[WSINCLEN+2];
double *dwsinc;

void nblint_gen_wsinc();

void nblint_init()
{
  if (initted)
    return;

  nblint_gen_wsinc();

  initted = 1;
}

void nblint_gen_wsinc( void )
{
  int i;

  wsinc = &wsinc_[1];
  printf("NAT pi %f\n", M_PI);

  for (i=0; i<WSINCLEN; ++i) {
    int ii=i-(WSINCLEN/2);
    double arg = ((double)ii) * (M_PI/Fo);
    double val = (arg==0.0) ? 1.0 : (sin(arg)/arg);
    wsinc[i] = val*HAMMING((i*M_PI)/(Nz*Fo));
  }

  // padding
  wsinc_[0] = wsinc_[WSINCLEN+1] = 0.0;

  // deltas
  dwsinc = &dwsinc_[1];
  for (i=0; i<WSINCLEN+2-1; ++i)
    dwsinc_[i] = wsinc_[i+1] - wsinc_[i];
  printf("NAT wsinc %f %f\n", wsinc[0], wsinc[1]);
  printf("NAT dwsinc %f %f\n", dwsinc[0], dwsinc[1]);
}

// nraw is src, raw is dest
void nblint_blint(double *raw, int len, double *nraw, int nlen) {
  //jboolean iscopy = JNI_FALSE;
  //jshort *raw, *nraw;
  //jsize len, nlen;
  int n, nn;
  double Fc;
  double Ww;
  int shorter;
  double ws_scale;
  //fix ws_scale_fx;
  double t, dt;
  double c0;
  double c1;
  double dwsincarg;

  printf("NAT0 ptrs %p %p\n", nraw, raw);
  printf("NAT %f %f\n", nraw[0], nraw[1]);

  nblint_init();

  // Copy the sinc to the dest, for debugging
  // memcpy(nraw, wsinc, (WSINCLEN+2) * sizeof(double));
  // return;

  /* nraw = (*env)->GetShortArrayElements( env, jnraw, &iscopy ); */
  /* raw = (*env)->GetShortArrayElements( env, jraw, &iscopy ); */

  /* nlen = (*env)->GetArrayLength( env, jnraw ); */
  /* len = (*env)->GetArrayLength( env, jraw ); */

  shorter = nlen < len;

  // half-width of window is
  Ww = shorter ? ((((double)Nz)*((double)len))/((double)nlen))
               : (double)Nz;

  // scale factor for sinc
  ws_scale = shorter ? ((double)nlen/(double)len) : 1.0;

  // output sample at n maps to input at
  t = 0;
  dt = (double)len/(double)nlen;

  c0 = (double)Fo*(Nz/Ww);
  c1 = (double)(Fo*Nz);
  dwsincarg = (Fo*Nz)/Ww;

  for (nn=0; nn<nlen; ++nn) {
    int startn, endn;
    double acc;
    int n;
    double wsincarg;

    // output sample at n maps to input at
    //t = ((double)nn*(double)len)/(double)nlen;

    startn = ceil( t-Ww );
    endn = floor( t+Ww );

    acc = 0;

    if (startn < 0)
      startn = 0;
    if (endn>=len)
      endn = len-1;

    wsincarg = ((double)startn-t)*c0;
    wsincarg = wsincarg + c1;

    for (n=startn; n<=endn; ++n) {
      double samp;
      int wsincarg_int;
      double wsincarg_frac;
      double wsinca, dws, ws;

      A(n>=0 && n<len);

      samp = raw[n];
      wsincarg_int = wsincarg;
      wsincarg_frac = wsincarg - wsincarg_int;
      wsinca = wsinc[wsincarg_int];
      dws = wsinc[wsincarg_int+1] - wsinca;
      ws = wsinca + (wsincarg_frac * dws);
      acc = acc + (samp * ws);

      wsincarg = wsincarg + dwsincarg;
    }

    A(nn>=0 && nn<nlen);
    nraw[nn] = acc * ws_scale;

    t += dt;
  }

  //printf( "OK\n" );

  fflush( stdout );
}

#if 0
JNIEXPORT void JNICALL Java_Nblint_native_1nshint
  (JNIEnv *env, jclass claz, jshortArray jnraw, jshortArray jraw)
{
  jboolean iscopy = JNI_FALSE;
  jshort *raw, *nraw;
  jsize len, nlen;
  double t, dt;
  int tp;

  nraw = (*env)->GetShortArrayElements( env, jnraw, &iscopy );
  raw = (*env)->GetShortArrayElements( env, jraw, &iscopy );

  //printf( "got %x %x\n", nraw, raw );

  nlen = (*env)->GetArrayLength( env, jnraw );
  len = (*env)->GetArrayLength( env, jraw );

  t = 0;
  dt = (double)len / (double)nlen;

  for (tp=0; tp<nlen; ++tp) {
    int ti = (int)t;

//    if (ti>=len)
//      break;
A(ti<len && ti>=0);

    nraw[tp] = raw[ti];

    t += dt;
  }

  (*env)->ReleaseShortArrayElements( env, jnraw, nraw, 0 );
  (*env)->ReleaseShortArrayElements( env, jraw, raw, 0 );

  //printf( "OK\n" );

  fflush( stdout );
}
#endif
